module PokerSpec where

import Test.Hspec

import Poker

spec :: Spec
spec = do
  describe "Poker" $ do
    describe "カードの文字列表記" $ do
      it "3♠" $ (show $ Card Spade Three) `shouldBe` "3♠"
      it "5♥" $ (show $ Card Heart Five) `shouldBe` "5♥"
      it "3♣" $ (show $ Card Club Three) `shouldBe` "3♣"
      it "K︎♦︎" $ (show $ Card Diamond King) `shouldBe` "K♦︎"

    describe "カードの比較" $ do 
      let threeOfSpades = Card Spade Three
      let aceOfSpades = Card Spade Ace
      let aceOfHearts = Card Heart Ace
      it "3♠とA♠は同じスートを持つ" $ (threeOfSpades `hasSameSuit` aceOfSpades) `shouldBe` True
      it "3♠とA♥は異なるスートを持つ" $ (threeOfSpades `hasSameSuit` aceOfHearts) `shouldBe` False
      it "3♠とA♠は異なるランクを持つ" $ (threeOfSpades `hasSameRank` aceOfSpades)　`shouldBe` False
      it "A♠とA♥は同じランクを持つ" $ (aceOfSpades `hasSameRank` aceOfHearts) `shouldBe` True
    
    describe "ツーカードのペアの役" $ do
      let aceOfSpades = Card Spade Ace
      let aceOfHearts = Card Heart Ace
      let kingOfSpades = Card Spade King
      let sevenOfDia = Card Diamond Seven
      let sevenOfSpades = Card Spade Seven
      let eightOfSpades = Card Spade Eight

      let cards = Cards aceOfSpades aceOfHearts
      it "ペア" $ hand cards `shouldBe` Pair

      let cards2 = Cards aceOfSpades sevenOfSpades
      it "フラッシュ" $ hand cards2 `shouldBe` Flush

      let cards3 = Cards sevenOfDia aceOfHearts
      it "ハイカード" $ hand cards3 `shouldBe` HighCard

      let cards4 = Cards kingOfSpades aceOfHearts
      it "ストレート" $ hand cards4 `shouldBe` Straight

      let cards5 = Cards sevenOfDia eightOfSpades
      it "ストレート(♦7,♠8)" $ hand cards5 `shouldBe` Straight

      let cards6 = Cards sevenOfSpades eightOfSpades
      it "ストレートフラッシュ(♠7,♠8)" $ hand cards6 `shouldBe` StraightFlush

    describe "役の強さを比較する" $ do 
      let aceOfSpades = Card Spade Ace
      let aceOfHearts = Card Heart Ace
      let kingOfSpades = Card Spade King
      let sevenOfDia = Card Diamond Seven
      let sevenOfSpades = Card Spade Seven
      let eightOfSpades = Card Spade Eight
      let twoOfHearts = Card Heart Two
      let twoOfSpades = Card Spade Two
      let threeOfSpades = Card Spade Three
      let threeOfHearts = Card Heart Three

      let pair = Cards aceOfSpades aceOfHearts
      let highCard = Cards sevenOfDia aceOfHearts

      let twoOfPair = Cards twoOfHearts twoOfSpades
      let threeOfPair = Cards threeOfHearts threeOfSpades
      let twoOfPair2 = Cards (Card Diamond Two) (Card Club Two)

      it "ハイカードとフラッシュで比較" $ do
        HighCard < Flush `shouldBe` True

      it "2つの手札が異なる場合" $ do
        pair > highCard `shouldBe` True

      it "Rankの強弱の比較" $ do
        Two < Six `shouldBe` True

      it "AceとTwoで比較" $ do
        Ace > Two `shouldBe` True
        Two < Ace `shouldBe` True

      it "AceとThreeで比較" $ do
        Ace > Three `shouldBe` True
        Three < Ace `shouldBe` True
      
      it "Ace同士" $ do 
        Ace > Ace `shouldBe` False
        Ace < Ace `shouldBe` False
        Ace >= Ace `shouldBe` True
        Ace <= Ace `shouldBe` True

      it "ペア同士の比較" $ do
        twoOfPair > threeOfPair `shouldBe` False
        threeOfPair > twoOfPair `shouldBe` True
        twoOfPair `compare` twoOfPair2 `shouldBe` EQ

      it "ストレート同士の比較" $ do 
        let straight = Cards eightOfSpades sevenOfDia
        let straight2 = Cards (Card Diamond Eight) (Card Spade Nine)
        let straight3 = Cards (Card Diamond Two) (Card Spade Ace)
        let straight4 = Cards (Card Heart Nine) (Card Club Eight)

        straight < straight2 `shouldBe` True
        straight2 < straight3 `shouldBe` False
        straight2 `compare` straight4 `shouldBe` EQ

      it "フラッシュ同士の比較" $ do
        let flush = Cards (Card Diamond Four) (Card Diamond Six)
        hand flush `shouldBe` Flush
        let flush2 = Cards (Card Heart Five) (Card Heart Seven)
        hand flush2 `shouldBe` Flush
        let flush3 = Cards (Card Diamond Four) (Card Diamond Seven)
        hand flush3 `shouldBe` Flush

        flush < flush2 `shouldBe` True
        flush3 < flush2 `shouldBe` True

      it "ハイカード同士の比較" $ do
        let highCard = Cards (Card Diamond Five) (Card Heart Seven)
        hand highCard `shouldBe` HighCard
        let highCard2 = Cards (Card Spade Four) (Card Diamond Seven)
        hand highCard2 `shouldBe` HighCard
        let highCard3 = Cards (Card Diamond Four) (Card Club Six)
        hand highCard3 `shouldBe` HighCard

        highCard > highCard2 `shouldBe` True
        highCard3 < highCard2 `shouldBe` True