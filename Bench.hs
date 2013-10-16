import Control.Exception (catch, SomeException(..))
import Control.Monad (forM_)
import Criterion.Main (bench, defaultMain)
import Data.Binary.Put (runPut, putWord32le)
import Data.Word (Word32)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Database.Sophia as S
import qualified System.Directory as Dir

bsWord :: Word32 -> BS.ByteString
bsWord = BS.concat . BSL.toChunks . runPut . putWord32le

ignoreExceptions :: IO () -> IO ()
ignoreExceptions act = act `catch` \(SomeException _) -> return ()

dbAccess :: IO ()
dbAccess = do
  ignoreExceptions $ Dir.removeDirectoryRecursive "/tmp/sophia-test-db"

  S.withEnv $ \env -> do
    S.openDir env S.ReadWrite S.AllowCreation "/tmp/sophia-test-db"
    S.withDb env $ \db -> do
      let many = forM_ [1..10000]
      many $ \i -> S.setValue db (bsWord i) (bsWord (i*2))
      many $ \i -> S.hasValue db (bsWord i)
      many $ \i -> S.getValue db (bsWord i)
      many $ \i -> S.delValue db (bsWord i)

main :: IO ()
main =
  defaultMain
  [ bench "DB access" dbAccess
  ]
