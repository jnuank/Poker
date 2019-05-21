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

  describe "ツーカード" $ do 
    describe "役の判定" $ do
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
          it "K♠/Q♦ < K♦/A♠" $ Cards c_kS c_qD < Cards c_kD c_aS `shouldBe` True
          it "2♦/3♠ > 2♦/A♠" $ Cards c_2D c_3S > Cards c_2D c_aS `shouldBe` True
          it "8♦/9♠ と 9♥/8♣ は引き分け" $ Cards c_8D c_9S `compare` Cards c_9H c_8C `shouldBe` EQ

        describe "フラッシュ同士の比較" $ do
          it "A◆/Q◆ > J♥/K♥" $  Cards c_aD c_qD > Cards c_jH c_kH `shouldBe` True
          it "4◆/2◆ < 3♥/5♥" $  Cards c_4D c_2D < Cards c_3H c_5H `shouldBe` True
          it "5◆/7◆ と 5♥/7♥ は引き分け" $  Cards c_5D c_7D `compare` Cards c_5H c_7H `shouldBe` EQ

        describe "ハイカード同士の比較" $ do
          it "A◆/Q♣ > J♠/K◆" $ Cards c_aD c_qC > Cards c_jS c_kD `shouldBe` True
          it "4◆/2♥ < 3♠/5◆" $ Cards c_4D c_2H < Cards c_3S c_5D `shouldBe` True
          it "5◆/7♥ と 5♠/7◆ は引き分け" $ Cards c_5D c_7H `compare` Cards c_5S c_7D `shouldBe` EQ

  describe "スリーカード" $ do 
    describe "役の判定" $ do
      it "A♥とA♣とA◆ はスリーカードである" $ threeHand (ThreeCards c_aH c_aC c_aD) `shouldBe` ThreeCard
      it "2♥と4♥と6♥ はフラッシュである" $ threeHand (ThreeCards c_2H c_4H c_6H) `shouldBe` Flush
    
      describe "ペア" $ do 
        it "K♥とK♣とA♥ はペアである" $ threeHand (ThreeCards c_kH c_kC c_aH) `shouldBe` Pair
        it "2♥と3♣と2◆ はペアである" $ threeHand (ThreeCards c_2H c_3C c_2D) `shouldBe` Pair
        it "2♥と3♣と3◆ はペアである" $ threeHand (ThreeCards c_2H c_3C c_3D) `shouldBe` Pair

      describe "ストレート" $ do 
        it "A♥とK♥とQ◆ はストレートである" $ threeHand  (ThreeCards c_aH c_kH c_qD) `shouldBe` Straight 
        it "K♥とA♥とQ◆ はストレートである" $ threeHand  (ThreeCards c_kH c_aH c_qD) `shouldBe` Straight 
        it "2♥とA♥と3◆ はストレートである" $ threeHand  (ThreeCards c_2H c_aH c_3D) `shouldBe` Straight 

      describe "ストレートフラッシュ" $ do
        it "A♥とK♥とQ♥ はストレートフラッシュである" $ threeHand  (ThreeCards c_aH c_kH c_qH) `shouldBe` StraightFlush 
        it "K◆とA◆とQ◆ はストレートフラッシュである" $ threeHand  (ThreeCards c_kD c_aD c_qD) `shouldBe` StraightFlush 
        it "2♠とA♠と3♠ はストレートフラッシュである" $ threeHand  (ThreeCards c_2S c_aS c_3S) `shouldBe` StraightFlush 
      
      describe "ハイカード" $ do 
        it "K♥と2♥とA♠ はハイカードである" $ threeHand (ThreeCards c_kH c_2H c_aS) `shouldBe` HighCard

    describe "手札の強さを比較する" $ do 
      describe "役が違う場合は、役の強さで強弱が決まる" $ do
        describe "ストレートフラッシュ > スリーカード" $ do 
          it "3♥/2♥/A♥ > A♠/A♣/A◆" $ ThreeCards c_3H c_2H c_aH > ThreeCards c_aS c_aC c_aD `shouldBe` True
      --   describe "ストレートフラッシュ > ペア" $ do
      --     it "A♠/K♠ > A♦/A♥" $ Cards c_aS c_kS > Cards c_aD c_aH `shouldBe` True
      --   describe "ペア > ストレート" $ do
      --     it "2♠/2♥ > A♦/K♣" $ Cards c_2S c_2H > Cards c_aD c_kC `shouldBe` True
      --   describe "ストレート > フラッシュ" $ do
      --     it "A♠/2♥ > A♦/3♦" $ Cards c_aS c_2H > Cards c_aD c_3D `shouldBe` True
      --   describe "フラッシュ > ハイカード" $ do
      --     it "2♠/4♠ > A♦/Q♣" $ Cards c_2S c_4S > Cards c_aD c_qC `shouldBe` True

      -- describe "役が同じ場合は、ランクの強弱で決まる" $ do
      --   describe "ストレートフラッシュ同士の比較" $ do
      --     it "A♠/K♠ > K♥/Q♥" $ Cards c_aS c_kS > Cards c_kH c_qH `shouldBe` True
      --     it "2♠/3♠ > A♥/2♥" $ Cards c_2S c_3S > Cards c_aH c_2H `shouldBe` True
      --     it "A♠/K♠ と A♥/K♥ は引き分け" $ Cards c_aC c_kC `compare` Cards c_aH c_kH `shouldBe` EQ

      --   describe "ペア同士の比較" $ do
      --     it "2♥/2♠ < 3♥/3♠" $ Cards c_2H c_2S > Cards c_3H c_3S `shouldBe` False
      --     it "A♥/A♠ > 2♥/2♠" $ Cards c_aH c_aS > Cards c_2H c_2S `shouldBe` True
      --     it "3♥/3♠ と 3♦/3♣ は引き分け" $ Cards c_3H c_3S `compare` Cards c_3D c_3C `shouldBe` EQ

      --   describe "ストレート同士の比較" $ do
      --     it "K♠/Q♦ < K♦/A♠" $ Cards c_kS c_qD < Cards c_kD c_aS `shouldBe` True
      --     it "2♦/3♠ > 2♦/A♠" $ Cards c_2D c_3S > Cards c_2D c_aS `shouldBe` True
      --     it "8♦/9♠ と 9♥/8♣ は引き分け" $ Cards c_8D c_9S `compare` Cards c_9H c_8C `shouldBe` EQ

      --   describe "フラッシュ同士の比較" $ do
      --     it "A◆/Q◆ > J♥/K♥" $  Cards c_aD c_qD > Cards c_jH c_kH `shouldBe` True
      --     it "4◆/2◆ < 3♥/5♥" $  Cards c_4D c_2D < Cards c_3H c_5H `shouldBe` True
      --     it "5◆/7◆ と 5♥/7♥ は引き分け" $  Cards c_5D c_7D `compare` Cards c_5H c_7H `shouldBe` EQ

      --   describe "ハイカード同士の比較" $ do
      --     it "A◆/Q♣ > J♠/K◆" $ Cards c_aD c_qC > Cards c_jS c_kD `shouldBe` True
      --     it "4◆/2♥ < 3♠/5◆" $ Cards c_4D c_2H < Cards c_3S c_5D `shouldBe` True
      --     it "5◆/7♥ と 5♠/7◆ は引き分け" $ Cards c_5D c_7H `compare` Cards c_5S c_7D `shouldBe` EQ


  describe "手札の中で最弱のランクを出す" $ do 
    it "Aと2と3 なら Aが最弱" $ lowestRank [Ace, Three, Two] `shouldBe` Ace
    it "AとKとQ なら Qが最弱" $ lowestRank [Ace, King, Queen] `shouldBe` Queen 
    it "Aと2なら     Aが最弱" $ lowestRank [Ace, Two] `shouldBe` Ace

  describe "orderingCardsWithHand" $ do 
    it "Aと2と3 なら 3,2,A" $ orderingCardsWithHand [c_aC, c_2D, c_3H] `shouldBe` [c_3H, c_2D, c_aC]
    it "Aと2と2 なら 2,2,A" $ orderingCardsWithHand [c_aC, c_2D, c_2H] `shouldBe` [c_2D, c_2H, c_aC]
    it "3と3と2 なら 3,3,2" $ orderingCardsWithHand [c_3C, c_3D, c_2H] `shouldBe` [c_3C, c_3D, c_2H]