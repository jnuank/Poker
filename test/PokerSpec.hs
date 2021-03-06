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
      it "7♠と8♠ はストレートフラッシュである" $ hand (TwoCards c_7S c_8S) `shouldBe` StraightFlush
      it "A♠とA♥ はペアである" $ hand (TwoCards c_aS c_aH) `shouldBe` Pair
      it "K♠とA♥ はストレートである" $ hand (TwoCards c_kS c_aH) `shouldBe` Straight
      it "7♦と8♠ はストレートである" $ hand (TwoCards c_7D c_8S) `shouldBe` Straight
      it "A♠と7♠ はフラッシュである" $ hand (TwoCards c_aS c_7S) `shouldBe` Flush
      it "7♦とA♥ はハイカードである" $ hand (TwoCards c_7D c_aH) `shouldBe` HighCard

    describe "手札の強さを比較する" $ do
      describe "役が違う場合は、役の強さで強弱が決まる" $ do
        describe "ストレートフラッシュ > ペア" $ do
          it "A♠/K♠ > A♦/A♥" $ TwoCards c_aS c_kS > TwoCards c_aD c_aH `shouldBe` True
        describe "ペア > ストレート" $ do
          it "2♠/2♥ > A♦/K♣" $ TwoCards c_2S c_2H > TwoCards c_aD c_kC `shouldBe` True
        describe "ストレート > フラッシュ" $ do
          it "A♠/2♥ > A♦/3♦" $ TwoCards c_aS c_2H > TwoCards c_aD c_3D `shouldBe` True
        describe "フラッシュ > ハイカード" $ do
          it "2♠/4♠ > A♦/Q♣" $ TwoCards c_2S c_4S > TwoCards c_aD c_qC `shouldBe` True

      describe "役が同じ場合は、ランクの強弱で決まる" $ do
        describe "ストレートフラッシュ同士の比較" $ do
          it "A♠/K♠ > K♥/Q♥" $ TwoCards c_aS c_kS > TwoCards c_kH c_qH `shouldBe` True
          it "2♠/3♠ > A♥/2♥" $ TwoCards c_2S c_3S > TwoCards c_aH c_2H `shouldBe` True
          it "A♠/K♠ と A♥/K♥ は引き分け" $ TwoCards c_aS c_kS `compare` TwoCards c_aH c_kH `shouldBe` EQ

        describe "ペア同士の比較" $ do
          it "2♥/2♠ < 3♥/3♠" $ TwoCards c_2H c_2S > TwoCards c_3H c_3S `shouldBe` False
          it "A♥/A♠ > 2♥/2♠" $ TwoCards c_aH c_aS > TwoCards c_2H c_2S `shouldBe` True
          it "3♥/3♠ と 3♦/3♣ は引き分け" $ TwoCards c_3H c_3S `compare` TwoCards c_3D c_3C `shouldBe` EQ

        describe "ストレート同士の比較" $ do
          it "K♠/Q♦ < K♦/A♠" $ TwoCards c_kS c_qD < TwoCards c_kD c_aS `shouldBe` True
          it "2♦/3♠ > 2♦/A♠" $ TwoCards c_2D c_3S > TwoCards c_2D c_aS `shouldBe` True
          it "8♦/9♠ と 9♥/8♣ は引き分け" $ TwoCards c_8D c_9S `compare` TwoCards c_9H c_8C `shouldBe` EQ

        describe "フラッシュ同士の比較" $ do
          it "A◆/Q◆ > J♥/K♥" $  TwoCards c_aD c_qD > TwoCards c_jH c_kH `shouldBe` True
          it "4◆/2◆ < 3♥/5♥" $  TwoCards c_4D c_2D < TwoCards c_3H c_5H `shouldBe` True
          it "5◆/7◆ と 5♥/7♥ は引き分け" $  TwoCards c_5D c_7D `compare` TwoCards c_5H c_7H `shouldBe` EQ

        describe "ハイカード同士の比較" $ do
          it "A◆/Q♣ > J♠/K◆" $ TwoCards c_aD c_qC > TwoCards c_jS c_kD `shouldBe` True
          it "4◆/2♥ < 3♠/5◆" $ TwoCards c_4D c_2H < TwoCards c_3S c_5D `shouldBe` True
          it "5◆/7♥ と 5♠/7◆ は引き分け" $ TwoCards c_5D c_7H `compare` TwoCards c_5S c_7D `shouldBe` EQ

  describe "スリーカード" $ do
    describe "役の判定" $ do
      it "A♥とA♣とA◆ はスリーカードである" $ hand (ThreeCards c_aH c_aC c_aD) `shouldBe` ThreeCard
      it "2♥と4♥と6♥ はフラッシュである" $ hand (ThreeCards c_2H c_4H c_6H) `shouldBe` Flush

      describe "ペア" $ do
        it "K♥とK♣とA♥ はペアである" $ hand (ThreeCards c_kH c_kC c_aH) `shouldBe` Pair
        it "2♥と3♣と2◆ はペアである" $ hand (ThreeCards c_2H c_3C c_2D) `shouldBe` Pair
        it "2♥と3♣と3◆ はペアである" $ hand (ThreeCards c_2H c_3C c_3D) `shouldBe` Pair

      describe "ストレート" $ do
        it "A♥とK♥とQ◆ はストレートである" $ hand  (ThreeCards c_aH c_kH c_qD) `shouldBe` Straight
        it "K♥とA♥とQ◆ はストレートである" $ hand  (ThreeCards c_kH c_aH c_qD) `shouldBe` Straight
        it "2♥とA♥と3◆ はストレートである" $ hand  (ThreeCards c_2H c_aH c_3D) `shouldBe` Straight

      describe "ストレートフラッシュ" $ do
        it "A♥とK♥とQ♥ はストレートフラッシュである" $ hand  (ThreeCards c_aH c_kH c_qH) `shouldBe` StraightFlush
        it "K◆とA◆とQ◆ はストレートフラッシュである" $ hand  (ThreeCards c_kD c_aD c_qD) `shouldBe` StraightFlush
        it "2♠とA♠と3♠ はストレートフラッシュである" $ hand  (ThreeCards c_2S c_aS c_3S) `shouldBe` StraightFlush

      describe "ハイカード" $ do
        it "K♥と2♥とA♠ はハイカードである" $ hand (ThreeCards c_kH c_2H c_aS) `shouldBe` HighCard

    describe "手札の強さを比較する" $ do
      describe "役が違う場合は、役の強さで強弱が決まる" $ do
        describe "ストレートフラッシュ > スリーカード" $ do
          it "3♥/2♥/A♥ > A♠/A♣/A◆" $ ThreeCards c_3H c_2H c_aH > ThreeCards c_aS c_aC c_aD `shouldBe` True
        describe "スリーカード > ストレート" $ do
          it "2♠/2♣/2◆ > A♥/K◆/Q♥" $ ThreeCards c_2S c_2C c_2D > ThreeCards c_aH c_kD c_qH `shouldBe` True
        describe "ストレート > フラッシュ" $ do
          it "3♥/2◆/A♣ > A♥/K♥/J♥" $ ThreeCards c_3H c_2D c_aC > ThreeCards c_aH c_kH c_jH `shouldBe` True
        describe "フラッシュ > ペア" $ do
          it "2◆/3◆/5◆ > A♣/A♥/K◆" $ ThreeCards c_2D c_3D c_5D > ThreeCards c_aC c_aH c_kD `shouldBe` True
        describe "ペア > ハイカード" $ do
          it "2♥/2◆/3◆ > A♥/Q◆/J♣" $ ThreeCards c_2H c_2D c_3D > ThreeCards c_aH c_qD c_jC `shouldBe` True

      describe "役が同じ場合は、ランクの強弱で決まる" $ do
        describe "ストレートフラッシュ同士の比較" $ do
          it "A♠/K♠/Q♠ > K♥/Q♥/J♥" $ ThreeCards c_aS c_kS c_qS > ThreeCards c_kH c_qH c_jH `shouldBe` True
          it "2♠/3♠/4♠ > A♥/2♥/3♥" $ ThreeCards c_2S c_3S c_4S > ThreeCards c_aH c_2H c_3H `shouldBe` True
          it "A♣/K♣/Q♣ と A♥/K♥/Q♥ は引き分け" $ ThreeCards c_aC c_kC c_qC `compare` ThreeCards c_aH c_kH c_qH `shouldBe` EQ

        describe "スリーカード同士の比較" $ do
          it "A♥/A◆/A♠ > K♥/K◆/K♠" $ ThreeCards c_aH c_aD c_aS > ThreeCards c_kH c_kD c_kS `shouldBe` True
          it "3♥/3◆/3♠ > 2♥/2◆/2♠" $ ThreeCards c_3H c_3D c_3S > ThreeCards c_2H c_2D c_2S `shouldBe` True

        describe "ストレート同士の比較" $ do
          it "A♠/K♠/Q◆ > K♥/Q♥/J◆" $ ThreeCards c_aS c_kS c_qD > ThreeCards c_kH c_qH c_jD `shouldBe` True
          it "2♠/3♠/4◆ > A♥/2♥/3◆" $ ThreeCards c_2S c_3S c_4D > ThreeCards c_aH c_2H c_3D `shouldBe` True
          it "A♣/K♣/Q◆ と A♥/K♥/Q♠ は引き分け" $ ThreeCards c_aC c_kC c_qD `compare` ThreeCards c_aH c_kH c_qS `shouldBe` EQ

        describe "フラッシュ同士の比較" $ do
          it "A♠/K♠/J♠ > K♥/Q♥/10♥" $ ThreeCards c_aS c_kS c_jS > ThreeCards c_kH c_qH c_10H `shouldBe` True
          it "2♠/4♠/5♠ > 5♥/2♥/3♥" $ ThreeCards c_2S c_4S c_5S > ThreeCards c_5H c_2H c_3H `shouldBe` True
          it "A♣/K♣/J♣ と A♥/K♥/J♥ は引き分け" $ ThreeCards c_aC c_kC c_jC `compare` ThreeCards c_aH c_kH c_jH `shouldBe` EQ

        describe "ペア同士の比較" $ do
          it "A♥/A◆/K♠ > K♥/K◆/A♠" $ ThreeCards c_aH c_aD c_kS > ThreeCards c_kH c_kD c_aS `shouldBe` True
          it "3♥/3◆/2♠ > 2♥/2◆/3♠" $ ThreeCards c_3H c_3D c_2S > ThreeCards c_2H c_2D c_3S `shouldBe` True
          it "3♥/3◆/2♠ と 2♥/3♣/3♠ は引き分け" $ ThreeCards c_3H c_3D c_2S `compare` ThreeCards c_2H c_3C c_3S `shouldBe` EQ

        describe "ハイカード同士の比較" $ do
          it "A◆/K♠/J♠ > K◆/Q♥/10♥" $ ThreeCards c_aD c_kS c_jS > ThreeCards c_kD c_qH c_10H `shouldBe` True
          it "2◆/4♠/5♠ > 5◆/2♥/3♥" $ ThreeCards c_2D c_4S c_5S > ThreeCards c_5D c_2H c_3H `shouldBe` True
          it "A◆/K♣/J♣ と A◆/K♥/J♥ は引き分け" $ ThreeCards c_aD c_kC c_jC `compare` ThreeCards c_aD c_kH c_jH `shouldBe` EQ

  describe "ファイブカード" $ do
    describe "役の判定" $ do
      it "A♥とK♥とQ♥とJ♥と10♥ はロイヤルストレートフラッシュである" $ hand (FiveCards c_aH c_kH c_qH c_jH c_10H) `shouldBe` RoyalStraightFlush
      it "K♥とQ♥とJ♥と10♥と9♥ はストレートフラッシュである" $ hand (FiveCards c_kH c_qH c_jH c_10H c_9H) `shouldBe` StraightFlush
      it "A♥とA♠とA◆とA♣と2♥  はフォーカードである" $ hand (FiveCards c_aH c_aS c_aD c_aC c_2H) `shouldBe` FourCard
      it "A♥とA♠とA◆と2♥と2◆  はフルハウスである" $ hand (FiveCards c_aH c_aS c_aD c_2H c_2D) `shouldBe` FullHouse
      it "A♥とK♥と3♥と2♥と5♥  はフラッシュである" $ hand (FiveCards c_aH c_kH c_3H c_2H c_5H) `shouldBe` Flush
      it "K♠とQ♥とJ♣と10◆と9♠ はストレートである" $ hand (FiveCards c_kS c_qH c_jC c_10D c_9S) `shouldBe` Straight
      it "A♥とA♠とA◆と2♣と3♥  はスリーカードである" $ hand (FiveCards c_aH c_aS c_aD c_2C c_3H) `shouldBe` ThreeCard
      it "A♥とA♠と2◆と2♣と3♥  はツーペアである" $ hand (FiveCards c_aH c_aS c_2D c_2C c_3H) `shouldBe` TwoPair
      it "A♥とA♠とQ◆と2♣と3♥  はワンペアである" $ hand (FiveCards c_aH c_aS c_qD c_2C c_3H) `shouldBe` Pair
      it "4♥と10♠とQ◆と2♣と3♥  はハイカードである" $ hand (FiveCards c_4H c_10S c_qD c_2C c_3H) `shouldBe` HighCard


    -- describe "手札の強さを比較する" $ do
    --   describe "役が違う場合は、役の強さで強弱が決まる" $ do
    --     describe "ストレートフラッシュ > スリーカード" $ do
    --       it "3♥/2♥/A♥ > A♠/A♣/A◆" $ ThreeCards c_3H c_2H c_aH > ThreeCards c_aS c_aC c_aD `shouldBe` True
    --     describe "スリーカード > ストレート" $ do
    --       it "2♠/2♣/2◆ > A♥/K◆/Q♥" $ ThreeCards c_2S c_2C c_2D > ThreeCards c_aH c_kD c_qH `shouldBe` True
    --     describe "ストレート > フラッシュ" $ do
    --       it "3♥/2◆/A♣ > A♥/K♥/J♥" $ ThreeCards c_3H c_2D c_aC > ThreeCards c_aH c_kH c_jH `shouldBe` True
    --     describe "フラッシュ > ペア" $ do
    --       it "2◆/3◆/5◆ > A♣/A♥/K◆" $ ThreeCards c_2D c_3D c_5D > ThreeCards c_aC c_aH c_kD `shouldBe` True
    --     describe "ペア > ハイカード" $ do
    --       it "2♥/2◆/3◆ > A♥/Q◆/J♣" $ ThreeCards c_2H c_2D c_3D > ThreeCards c_aH c_qD c_jC `shouldBe` True

    --   describe "役が同じ場合は、ランクの強弱で決まる" $ do
    --     describe "ストレートフラッシュ同士の比較" $ do
    --       it "A♠/K♠/Q♠ > K♥/Q♥/J♥" $ ThreeCards c_aS c_kS c_qS > ThreeCards c_kH c_qH c_jH `shouldBe` True
    --       it "2♠/3♠/4♠ > A♥/2♥/3♥" $ ThreeCards c_2S c_3S c_4S > ThreeCards c_aH c_2H c_3H `shouldBe` True
    --       it "A♣/K♣/Q♣ と A♥/K♥/Q♥ は引き分け" $ ThreeCards c_aC c_kC c_qC `compare` ThreeCards c_aH c_kH c_qH `shouldBe` EQ

    --     describe "スリーカード同士の比較" $ do
    --       it "A♥/A◆/A♠ > K♥/K◆/K♠" $ ThreeCards c_aH c_aD c_aS > ThreeCards c_kH c_kD c_kS `shouldBe` True
    --       it "3♥/3◆/3♠ > 2♥/2◆/2♠" $ ThreeCards c_3H c_3D c_3S > ThreeCards c_2H c_2D c_2S `shouldBe` True

    --     describe "ストレート同士の比較" $ do
    --       it "A♠/K♠/Q◆ > K♥/Q♥/J◆" $ ThreeCards c_aS c_kS c_qD > ThreeCards c_kH c_qH c_jD `shouldBe` True
    --       it "2♠/3♠/4◆ > A♥/2♥/3◆" $ ThreeCards c_2S c_3S c_4D > ThreeCards c_aH c_2H c_3D `shouldBe` True
    --       it "A♣/K♣/Q◆ と A♥/K♥/Q♠ は引き分け" $ ThreeCards c_aC c_kC c_qD `compare` ThreeCards c_aH c_kH c_qS `shouldBe` EQ

    --     describe "フラッシュ同士の比較" $ do
    --       it "A♠/K♠/J♠ > K♥/Q♥/10♥" $ ThreeCards c_aS c_kS c_jS > ThreeCards c_kH c_qH c_10H `shouldBe` True
    --       it "2♠/4♠/5♠ > 5♥/2♥/3♥" $ ThreeCards c_2S c_4S c_5S > ThreeCards c_5H c_2H c_3H `shouldBe` True
    --       it "A♣/K♣/J♣ と A♥/K♥/J♥ は引き分け" $ ThreeCards c_aC c_kC c_jC `compare` ThreeCards c_aH c_kH c_jH `shouldBe` EQ

    --     describe "ペア同士の比較" $ do
    --       it "A♥/A◆/K♠ > K♥/K◆/A♠" $ ThreeCards c_aH c_aD c_kS > ThreeCards c_kH c_kD c_aS `shouldBe` True
    --       it "3♥/3◆/2♠ > 2♥/2◆/3♠" $ ThreeCards c_3H c_3D c_2S > ThreeCards c_2H c_2D c_3S `shouldBe` True
    --       it "3♥/3◆/2♠ と 2♥/3♣/3♠ は引き分け" $ ThreeCards c_3H c_3D c_2S `compare` ThreeCards c_2H c_3C c_3S `shouldBe` EQ

    --     describe "ハイカード同士の比較" $ do
    --       it "A◆/K♠/J♠ > K◆/Q♥/10♥" $ ThreeCards c_aD c_kS c_jS > ThreeCards c_kD c_qH c_10H `shouldBe` True
    --       it "2◆/4♠/5♠ > 5◆/2♥/3♥" $ ThreeCards c_2D c_4S c_5S > ThreeCards c_5D c_2H c_3H `shouldBe` True
    --       it "A◆/K♣/J♣ と A◆/K♥/J♥ は引き分け" $ ThreeCards c_aD c_kC c_jC `compare` ThreeCards c_aD c_kH c_jH `shouldBe` EQ
