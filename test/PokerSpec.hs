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