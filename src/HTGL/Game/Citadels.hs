{-# LANGUAGE OverloadedStrings #-}

module HTGL.Game.Citadels where

import Control.Lens
import Data.String
import Data.Text
import qualified Data.Map as M
import HTGL.Play
import HTGL.Color

data Color = Red
           | Blue
           | Green
           | Yellow
           | Purple
           deriving (Show,Eq,Ord)

instance Styling Color where
    styling Red = bold <> red 
    styling Blue = bold <> blue
    styling Green = bold <> green
    styling Yellow = bold <> yellow
    styling Purple = magenta

data Role = Assassin 
          | Thief 
          | Mage
          | King
          | Merchant
          | Bishop
          | Architect
          | Condottiere
          deriving (Show, Eq, Ord)

roleColor :: Role -> Maybe Color
roleColor King = Just Yellow
roleColor Merchant = Just Green
roleColor Bishop = Just Blue
roleColor Condottiere = Just Red
roleColor _ = Nothing

instance Styling Role where
    styling = maybe (bold<>white) styling . roleColor


data Card = Card {
    name :: Text,
    cardColor :: Color,
    cost :: Integer
} deriving (Show, Eq, Ord)

deck :: [Card]
deck = [
    Card "Observatory" Purple 5,
    Card "Court of Miracles" Purple 2,
    Card "Castle" Yellow 4,
    Card "Castle" Yellow 4,
    Card "Wishing Well" Purple 5,
    Card "Cathedral" Blue 5,
    Card "Factory" Purple 5,
    Card "City Hall" Green 5,
    Card "Port" Green 4,
    Card "Monastery" Blue 3,
    Card "Monastery" Blue 3,
    Card "Monastery" Blue 3,
    Card "Counter" Green 3,
    Card "Counter" Green 3,
    Card "Counter" Green 3,
    Card "Market" Green 2,
    Card "Market" Green 2,
    Card "Market" Green 2,
    Card "Prison" Red 2,
    Card "Prison" Red 2,
    Card "Shoppe" Green 2,
    Card "Shoppe" Green 2,
    Card "Temple" Blue 1,
    Card "Church" Blue 2,
    Card "Church" Blue 2,
    Card "Palace" Yellow 5,
    Card "Quarry" Purple 5,
    Card "Tavern" Green 1,
    Card "Tavern" Green 1,
    Card "Fortress" Red 5,
    Card "Barracks" Red 3 ]





instance Colorful Card where
    colorful (Card name color cost) = (name <> " (" <> fromString (show cost) <> ")") `withStyle` styling color

data GameData = GameData {
    _roles :: M.Map Player Role,
    _players :: [Player]
}

{-
game = do
    announce "Character selection round starts !"


selectionTurn = do
    currentPlayer `chooses` 
-}
