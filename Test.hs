{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty.HUnit ((@?=))
import qualified Database.Sophia as S
import qualified System.Directory as Dir
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as TUnit

main :: IO ()
main = T.defaultMain tests

tests :: T.TestTree
tests =
  T.testGroup "Unit tests"
  [ TUnit.testCase "Create DB, call some APIs" $ do
      Dir.removeDirectoryRecursive "/tmp/sophia-test-db"

      putStrLn "Phase 1"
      env <- S.createEnv
      S.openDir env S.ReadWrite S.AllowCreation "/tmp/sophia-test-db"
      db <- S.openDb env
      TUnit.assertBool "Key must not exist" . not =<< S.hasValue db "Key"
      do
        res <- S.getValue db "Key"
        res @?= Nothing

      putStrLn "Phase 2"
      S.setValue db "Key" "Val"
      TUnit.assertBool "Key2 must not exist" . not =<< S.hasValue db "Key2"
      TUnit.assertBool "Key must exist" =<< S.hasValue db "Key"
      do
        res <- S.getValue db "Key"
        res @?= Just "Val"

      putStrLn "Phase 3"
      S.setValue db "A" "foo"
      S.setValue db "Z" "bar"

      S.withCursor db S.GTE "A" $ \cursor -> do
        res <- S.fetchCursorAll cursor
        res @?=
          [ ("A", "foo")
          , ("Key", "Val")
          , ("Z", "bar")
          ]

      putStrLn "Phase 4"
      putStrLn "Deleting \"Key\""
      S.delValue db "Key"
      putStrLn "Done deleting"
      TUnit.assertBool "Key must not exist" . not =<< S.hasValue db "Key"

      putStrLn "Done!"
  ]
