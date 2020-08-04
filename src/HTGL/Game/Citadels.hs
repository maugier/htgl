{-# LANGUAGE OverloadedStrings #-}

module HTGL.Game.Citadels where

import Control.Lens
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

instance Colorful Color where
    colorful Red = bold.red $ "red"
    colorful Blue = blue $ "blue"
    colorful Green = bold.green $ "green"
    colorful Yellow = bold.yellow $ "yellow"
    colorful Purple = magenta $ "purple"

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
