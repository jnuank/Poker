module PokerSpec where

import Test.Hspec

import Poker
import Card

spec :: Spec
spec = do
  let
    [   c_aS, c_2S, c_3S, c_4S, c_5S, c_6S, c_7S, c_8S, c_9S, c_10S, c_jS, c_qS, c_kS
      , c_aH, c_2H, c_3H, c_4H, c_5H, c_6H, c_7H, c_8H, c_9H, c_10H, c_jH, c_qH, c_kH
      , c_aC, c_2C, c_3C, c_4C, c_5C, c_6C, c_7C, c_8C, c_9C, c_10C, c_jC, c_qC, c_kC
      , c_aD, c_2D, c_3D, c_4D, c_5D, c_6D, c_7D, c_8D, c_9D, c_10D, c_jD, c_qD, c_kD]
      = [Card suit rank | suit <- [Spade, Heart, Club, Diamond], rank <- [Ace .. King]]

  describe "ツーカードのペアの役" $ do
    it "7♠と8♠ はストレートフラッシュである" $ hand (Cards c_7S c_8S) `shouldBe` StraightFlush
    it "A♠とA♥ はペアである" $ hand (Cards c_aS c_aH) `shouldBe` Pair
    it "K♠とA♥ はストレートである" $ hand (Cards c_kS c_aH) `shouldBe` Straight
    it "7♦と8♠ はストレートである" $ hand (Cards c_7D c_8S) `shouldBe` Straight
    it "A♠と7♠ はフラッシュである" $ hand (Cards c_aS c_7S) `shouldBe` Flush
    it "7♦とA♥ はハイカードである" $ hand (Cards c_7D c_aH) `shouldBe` HighCard

  describe "手札の強さを比較する" $ do 
    describe "役が違う場合は、役の強さで強弱が決まる" $ do
      describe "ストレートフラッシュ > ペア" $ do
        it "A♠/K♠ > A♦/A♥" $ Cards c_aS c_kS > Cards c_aD c_aH `shouldBe` True
      describe "ペア > ストレート" $ do
        it "2♠/2♥ > A♦/K♣" $ Cards c_2S c_2H > Cards c_aD c_kC `shouldBe` True
      describe "ストレート > フラッシュ" $ do
        it "A♠/2♥ > A♦/3♦" $ Cards c_aS c_2H > Cards c_aD c_3D `shouldBe` True
      describe "フラッシュ > ハイカード" $ do
        it "2♠/4♠ > A♦/Q♣" $ Cards c_2S c_4S > Cards c_aD c_qC `shouldBe` True

    describe "役が同じ場合は、ランクの強弱で決まる" $ do
      describe "ストレートフラッシュ同士の比較" $ do
        it "A♠/K♠ > K♥/Q♥" $ Cards c_aS c_kS > Cards c_kH c_qH `shouldBe` True
        it "2♠/3♠ > A♥/2♥" $ Cards c_2S c_3S > Cards c_aH c_2H `shouldBe` True
        it "A♠/K♠ と A♥/K♥ は引き分け" $ Cards c_aC c_kC `compare` Cards c_aH c_kH `shouldBe` EQ

      describe "ペア同士の比較" $ do
        it "2♥/2♠ < 3♥/3♠" $ Cards c_2H c_2S > Cards c_3H c_3S `shouldBe` False
        it "A♥/A♠ > 2♥/2♠" $ Cards c_aH c_aS > Cards c_2H c_2S `shouldBe` True
        it "3♥/3♠ と 3♦/3♣ は引き分け" $ Cards c_3H c_3S `compare` Cards c_3D c_3C `shouldBe` EQ

      describe "ストレート同士の比較" $ do
        it "8♠/7♦ < 8♦/9♠" $ Cards c_8S c_7D < Cards c_8D c_9S `shouldBe` True
        it "8♦/9♠ > 2♦/A♠" $ Cards c_8D c_9S > Cards c_2D c_aS `shouldBe` True
        it "8♦/9♠ と 9♥/8♣ は引き分け" $ Cards c_8D c_9S `compare` Cards c_9H c_8C `shouldBe` EQ

      describe "フラッシュ同士の比較" $ do
        it "4◆/6◆ < 5♥/7♥" $  Cards c_4D c_6D < Cards c_5H c_7H `shouldBe` True
        it "4◆/V7◆ < 5♥/7♥" $  Cards c_4D c_7D < Cards c_5H c_7H `shouldBe` True
        it "5◆/7◆ と 5♥/7♥ は引き分け" $  Cards c_5D c_7D `compare` Cards c_5H c_7H `shouldBe` EQ

      describe "ハイカード同士の比較" $ do
        it "4◆/6♣ < 4♠/7◆" $ Cards c_4D c_6C < Cards c_4S c_7D `shouldBe` True
        it "5◆/7♥ > 4♠/7◆" $ Cards c_5D c_7H > Cards c_4S c_7D `shouldBe` True
        it "5◆/7♥ と 5♠/7◆ は引き分け" $ Cards c_5D c_7H `compare` Cards c_5S c_7D `shouldBe` EQ
