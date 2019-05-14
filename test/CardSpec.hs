module CardSpec where

import Test.Hspec

import Card

spec :: Spec
spec = do
  let
    [   c_aS, c_2S, c_3S, c_4S, c_5S, c_6S, c_7S, c_8S, c_9S, c_10S, c_jS, c_qS, c_kS
      , c_aH, c_2H, c_3H, c_4H, c_5H, c_6H, c_7H, c_8H, c_9H, c_10H, c_jH, c_qH, c_kH
      , c_aC, c_2C, c_3C, c_4C, c_5C, c_6C, c_7C, c_8C, c_9C, c_10C, c_jC, c_qC, c_kC
      , c_aD, c_2D, c_3D, c_4D, c_5D, c_6D, c_7D, c_8D, c_9D, c_10D, c_jD, c_qD, c_kD]
      = [Card suit rank | suit <- [Spade, Heart, Club, Diamond], rank <- [Ace .. King]]

  describe "カードの文字列表記" $ do
    it "3♠" $ (show c_3S) `shouldBe` "3♠"
    it "A♠" $ (show c_aS) `shouldBe` "A♠"
    it "J♥" $ (show c_jH) `shouldBe` "J♥"
    it "Q♣" $ (show c_qC) `shouldBe` "Q♣"
    it "K︎♦︎" $ (show c_kD) `shouldBe` "K♦︎"

  describe "カードの比較" $ do 
    describe "スートの比較" $ do
      it "3♠とA♠は同じスートを持つ" $ (c_3S `hasSameSuit` c_aS) `shouldBe` True
      it "3♠とA♥は異なるスートを持つ" $ (c_3S `hasSameSuit` c_aH) `shouldBe` False

    describe "ランクの比較" $ do
      it "3♠とA♠は異なるランクを持つ" $ (c_3S `hasSameRank` c_aS) `shouldBe` False
      it "A♠とA♥は同じランクを持つ" $ (c_aS `hasSameRank` c_aH) `shouldBe` True

