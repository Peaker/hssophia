module Database.Sophia.Types where

import qualified Bindings.Sophia as S

newtype Db = Db { unDb :: S.Db }
newtype Env = Env { unEnv :: S.Env }
newtype Cursor = Cursor { unCursor :: S.Cursor }
