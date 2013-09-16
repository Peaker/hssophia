{-# LANGUAGE DeriveDataTypeable #-}
module Database.Sophia
  ( createEnv
    , CreateEnvFailed(..), SetKeyComparisonFailed(..)
    , Env

  , IOMode(..), AllowCreation(..)
  , openDir , OpenDirFailed(..)
  , openDb  , OpenDbFailed(..)
    , Db
  , hasValue, HasValueFailed(..)
  , getValue, GetValueFailed(..)
  , setValue, SetValueFailed(..)
  , delValue, DelValueFailed(..)

  , Order(..)
  , createCursor, createCursorAt
    , CreateCursorFailed(..)
    , Cursor

  , fetchCursor, FetchCursorFailed(..)
  , keyAtCursor, valAtCursor, AtCursorFailed(..)

  -- Convenience wrapper:
  , fetchCursorAll
  ) where

import Prelude hiding (Ordering(..))

import Control.Applicative (Applicative(..), (<$>))
import Control.Monad (void, when)
import Data.Bits ((.|.))
import Data.ByteString (ByteString, packCStringLen)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen, unsafePackMallocCStringLen)
import Data.Typeable (Typeable)
import Database.Sophia.Types
import Foreign.C.String (withCString, peekCString)
import Foreign.C.Types (CInt, CUInt, CSize(..), CChar)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, FunPtr, nullPtr, castPtr)
import Foreign.Storable (peek)
import qualified Bindings.Sophia as S
import qualified Control.Exception as E

foreign import ccall "sophia.h &sp_destroy"
  -- we're forced to re-import sp_destroy_ptr just to ignore destroy's
  -- error code here:
   sp_destroy_ptr :: FunPtr (S.Handle -> IO ())

throwErrorIf ::
  E.Exception exc => S.Handle -> (a -> Bool) -> (String -> exc) -> IO a -> IO a
throwErrorIf h isErr mkErr action = do
  res <- action
  if isErr res
    then E.throwIO . mkErr =<< peekCString =<< S.c'sp_error h
    else return res

throwErrorIfNeg ::
  E.Exception exc =>
  S.Handle -> (String -> exc) -> IO CInt -> IO CInt
throwErrorIfNeg h mkErr act = throwErrorIf h (< 0) mkErr act

throwErrorIfNotZero ::
  E.Exception exc =>
  S.Handle -> (String -> exc) -> IO CInt -> IO ()
throwErrorIfNotZero h mkErr act = void $ throwErrorIf h (/= 0) mkErr act

throwErrorIfNull ::
  E.Exception exc =>
  S.Handle -> (String -> exc) -> IO (Ptr a) -> IO (Ptr a)
throwErrorIfNull h mkErr = throwErrorIf h (nullPtr ==) mkErr

data CreateEnvFailed = CreateEnvFailed String deriving (Show, Typeable)
instance E.Exception CreateEnvFailed

data SetKeyComparisonFailed = SetKeyComparisonFailed String deriving (Show, Typeable)
instance E.Exception SetKeyComparisonFailed

foreign import ccall "lexical_cmp.h &sp_compare_lexicographically"
   sp_compare_lexicographically :: FunPtr (Ptr CChar -> CSize -> Ptr CChar -> CSize -> Ptr () -> IO CInt)

destroyer :: (ForeignPtr () -> a) -> Ptr () -> IO a
destroyer cons = fmap cons . newForeignPtr sp_destroy_ptr

createEnv :: IO Env
createEnv = do
  envPtr <- S.c'sp_env
  if envPtr == nullPtr
    then -- can't use sp_error yet...
      E.throwIO $ CreateEnvFailed "Sophia.createEnv failed (likely allocation failure)"
    else do
      throwErrorIfNotZero envPtr SetKeyComparisonFailed $
        S.c'sp_set_key_comparison envPtr sp_compare_lexicographically nullPtr
      destroyer Env envPtr

data IOMode = ReadOnly | ReadWrite
data AllowCreation = AllowCreation | DisallowCreation

ioModeFlags :: IOMode -> S.Flags
ioModeFlags ReadOnly = S.c'SPO_RDONLY
ioModeFlags ReadWrite = S.c'SPO_RDWR

allowCreationFlags :: AllowCreation -> S.Flags
allowCreationFlags AllowCreation = S.c'SPO_CREAT
allowCreationFlags DisallowCreation = 0

withEnv :: Env -> (S.Env -> IO a) -> IO a
withEnv = withForeignPtr . unEnv

withDb :: Db -> (S.Db -> IO a) -> IO a
withDb = withForeignPtr . unDb

withCursor :: Cursor -> (S.Cursor -> IO a) -> IO a
withCursor = withForeignPtr . unCursor

data OpenDirFailed = OpenDirFailed String deriving (Show, Typeable)
instance E.Exception OpenDirFailed

openDir :: Env -> IOMode -> AllowCreation -> FilePath -> IO ()
openDir env ioMode allowCreation path =
  withEnv env $ \cEnv ->
  withCString path $ \cPath ->
  throwErrorIfNotZero cEnv OpenDirFailed $ S.c'sp_dir cEnv flags cPath
  where
    flags = ioModeFlags ioMode .|. allowCreationFlags allowCreation

data OpenDbFailed = OpenDbFailed String deriving (Show, Typeable)
instance E.Exception OpenDbFailed

openDb :: Env -> IO Db
openDb env =
  withEnv env $ \cEnv ->
  do
    dbPtr <- throwErrorIfNull cEnv OpenDbFailed $ S.c'sp_open cEnv
    destroyer Db dbPtr

data HasValueFailed = HasValueFailed String deriving (Show, Typeable)
instance E.Exception HasValueFailed

withByteString :: ByteString -> ((S.Key, CSize) -> IO a) -> IO a
withByteString bs f =
  unsafeUseAsCStringLen bs $ \(cKey, keyLen) ->
  f (castPtr cKey, fromIntegral keyLen)

hasValue :: Db -> ByteString -> IO Bool
hasValue db key =
  withDb db $ \cDb ->
  withByteString key $ \(cKey, keyLen) ->
  do
    res <-
      throwErrorIfNeg cDb HasValueFailed $
      S.c'sp_get cDb cKey keyLen nullPtr nullPtr
    return $ res /= 0

data GetValueFailed = GetValueFailed String deriving (Show, Typeable)
instance E.Exception GetValueFailed

getValue :: Db -> ByteString -> IO (Maybe ByteString)
getValue db key =
  withDb db $ \cDb ->
  withByteString key $ \(cKey, keyLen) ->
  alloca $ \cPtrPtr ->
  alloca $ \cLenPtr ->
  do
    res <-
      throwErrorIfNeg cDb GetValueFailed $
      S.c'sp_get cDb cKey  keyLen cPtrPtr cLenPtr
    if res == 0
      then return Nothing
      else Just <$> do
        cPtr <- peek cPtrPtr
        cLen <- peek cLenPtr
        unsafePackMallocCStringLen (castPtr cPtr, fromIntegral cLen)

data SetValueFailed = SetValueFailed String deriving (Show, Typeable)
instance E.Exception SetValueFailed

setValue :: Db -> ByteString -> ByteString -> IO ()
setValue db key val =
  withDb db $ \cDb ->
  withByteString key $ \(cKey, keyLen) ->
  withByteString val $ \(cVal, valLen) ->
  throwErrorIfNotZero cDb SetValueFailed $
  S.c'sp_set cDb cKey keyLen cVal valLen

data DelValueFailed = DelValueFailed String deriving (Show, Typeable)
instance E.Exception DelValueFailed

delValue :: Db -> ByteString -> IO ()
delValue db key =
  withDb db $ \cDb ->
  withByteString key $ \(cKey, keyLen) ->
  throwErrorIfNotZero cDb DelValueFailed $
  S.c'sp_delete cDb cKey keyLen

data Order = GT | GTE | LT | LTE

data CreateCursorFailed = CreateCursorFailed String deriving (Show, Typeable)
instance E.Exception CreateCursorFailed

cOrder :: Order -> CUInt
cOrder GT  = S.c'SPGT
cOrder LT  = S.c'SPLT
cOrder GTE = S.c'SPGTE
cOrder LTE = S.c'SPLTE

createCursorH :: S.Key -> CSize -> Db -> Order -> IO Cursor
createCursorH cKey keyLen db order =
  withDb db $ \cDb -> do
    cursorPtr <-
      throwErrorIfNull cDb CreateCursorFailed $
      S.c'sp_cursor cDb (cOrder order) cKey keyLen
    destroyer Cursor cursorPtr

createCursor :: Db -> Order -> IO Cursor
createCursor = createCursorH nullPtr 0

createCursorAt :: Db -> Order -> ByteString -> IO Cursor
createCursorAt db order key =
  withByteString key $ \(cKey, keyLen) ->
  createCursorH cKey keyLen db order

data FetchCursorFailed = FetchCursorFailed deriving (Show, Typeable)
instance E.Exception FetchCursorFailed

fetchCursor :: Cursor -> IO Bool
fetchCursor cursor =
  withCursor cursor $ \cCursor -> do
    res <- S.c'sp_fetch cCursor
    -- Docs say fetch can't fail, and it doesn't fill error str, but
    -- it does return -1 in some cases (without err str)
    when (res < 0) $ E.throwIO FetchCursorFailed
    return (res /= 0)

data AtCursorFailed = AtCursorFailed deriving (Show, Typeable)
instance E.Exception AtCursorFailed

atCursor ::
  (S.Cursor -> IO (Ptr ())) ->
  (S.Cursor -> IO CSize) ->
  Cursor -> IO ByteString
atCursor cGetStr cGetLen cursor =
  withCursor cursor $ \cCursor -> do
    cKey <- cGetStr cCursor
    keyLen <- cGetLen cCursor
    when (nullPtr == cKey  ) $ E.throwIO AtCursorFailed
    when (0       == keyLen) $ E.throwIO AtCursorFailed
    -- Must use O(N) copy here? Not sure what the memory semantics of
    -- sp_cursor/sp_fetch/sp_key/sp_destroy are, so for now the answer
    -- is STAY SAFE:
    packCStringLen (castPtr cKey, fromIntegral keyLen)

keyAtCursor :: Cursor -> IO ByteString
keyAtCursor = atCursor S.c'sp_key S.c'sp_keysize

valAtCursor :: Cursor -> IO ByteString
valAtCursor = atCursor S.c'sp_value S.c'sp_valuesize

fetchCursorAll :: Cursor -> IO [(ByteString, ByteString)]
fetchCursorAll cursor = do
  more <- fetchCursor cursor
  if more
    then do
      pair <- (,) <$> keyAtCursor cursor <*> valAtCursor cursor
      rest <- fetchCursorAll cursor
      return $ pair : rest
    else
      return []
