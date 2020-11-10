module ASpec where

import           A
import           Polysemy
import           Polysemy.Reader
import           Test.Hspec

test1 :: IO Int -- tested
test1 = runM . runReader 0 . runI $ store' 42

test2 :: IO Int -- not tested, but compiles
test2 = runM . runReader 0 . runI $ read' >>= store'

spec :: Spec
spec = describe "Running" $ do
  describe "Storing" $
    it "returns what i give it" $ do
      r <- test1
      r `shouldBe` 42
  describe "Storing & Loading" $
    it "returns what i give it" $ do
      r <- (runM . runReader 31 . runI $ loadFromEnv)
      r `shouldBe` 33 -- there's a (+2) applied.



