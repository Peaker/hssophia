module Database.Sophia.Types where

import Foreign.ForeignPtr (ForeignPtr)
import qualified Bindings.Sophia as S

newtype Db = Db { unDb :: ForeignPtr () }
newtype Env = Env { unEnv :: ForeignPtr () }
newtype Cursor = Cursor { unCursor :: S.Cursor }
