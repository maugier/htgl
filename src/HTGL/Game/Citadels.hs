{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module HTGL.Game.Citadels where

import Control.Lens
import Control.Lens.TH
import Control.Monad
import Control.Monad.State (StateT, evalStateT, get)
import Data.String
import Data.Text (Text)
import Data.List (sort, reverse)
import qualified Data.Map as M
import HTGL.Play
import HTGL.Color

{- Citadelles, a card game by Bruno Faidutti -}


{- Game elements -}


-- Some game items have a color property.
data Color = Red
           | Blue
           | Green
           | Yellow
           | Purple
           deriving (Show,Eq,Ord,Enum,Bounded)

colorStyle :: Color -> Style
colorStyle Red = bold <> red
colorStyle Blue = bold <> blue
colorStyle Green = bold <> green
colorStyle Yellow = bold <> yellow
colorStyle Purple = magenta

instance Colorful Color where
    colorful color = color `showStyle` colorStyle color


-- We have 8 character cards. Order is important.
data Role = Assassin 
          | Thief 
          | Mage
          | King
          | Merchant
          | Bishop
          | Architect
          | Condottiere
          deriving (Show, Eq, Ord, Enum)

-- Some role cards, but not all, bear a specific color.
roleColor :: Role -> Maybe Color
roleColor King = Just Yellow
roleColor Merchant = Just Green
roleColor Bishop = Just Blue
roleColor Condottiere = Just Red
roleColor _ = Nothing

instance Colorful Role where
    colorful role = role `showStyle` maybe (bold<>white) colorStyle (roleColor role)

-- The game uses cards. Each card has a name, a color, and a cost indicated in gold pieces.
data Card = Card {
    name :: !Text,
    cardColor :: !Color,
    cost :: !Int
} deriving (Show, Eq, Ord)

-- Here is the standard deck of cards.
deck :: [Card]
deck = [
    Card "Watchtower" Red 1,
    Card "Watchtower" Red 1,
    Card "Watchtower" Red 1,
    Card "Prison" Red 2,
    Card "Prison" Red 2,
    Card "Barracks" Red 3,
    Card "Barracks" Red 3,
    Card "Barracks" Red 3,
    Card "Fortress" Red 5,
    Card "Fortress" Red 5,
    Card "Fortress" Red 5,
--
    Card "Temple" Blue 1,
    Card "Temple" Blue 1,
    Card "Temple" Blue 1,
    Card "Church" Blue 2,
    Card "Church" Blue 2,
    Card "Church" Blue 2,
    Card "Monastery" Blue 3,
    Card "Monastery" Blue 3,
    Card "Monastery" Blue 3,
    Card "Monastery" Blue 3,
    Card "Cathedral" Blue 5,
    Card "Cathedral" Blue 5,
--
    Card "Tavern" Green 1,
    Card "Tavern" Green 1,
    Card "Tavern" Green 1,
    Card "Tavern" Green 1,
    Card "Tavern" Green 1,
    Card "Market" Green 2,
    Card "Market" Green 2,
    Card "Market" Green 2,
    Card "Market" Green 2,
    Card "Shoppe" Green 2,
    Card "Shoppe" Green 2,
    Card "Shoppe" Green 2,
    Card "Shoppe" Green 2,
    Card "Counter" Green 3,
    Card "Counter" Green 3,
    Card "Counter" Green 3,
    Card "Port" Green 4,
    Card "Port" Green 4,
    Card "Port" Green 4,
    Card "City Hall" Green 5,
    Card "City Hall" Green 5,
--
    Card "Manor" Yellow 3,
    Card "Manor" Yellow 3,
    Card "Manor" Yellow 3,
    Card "Manor" Yellow 3,
    Card "Manor" Yellow 3,
    Card "Castle" Yellow 4,
    Card "Castle" Yellow 4,
    Card "Castle" Yellow 4,
    Card "Castle" Yellow 4,
    Card "Castle" Yellow 4,
    Card "Palace" Yellow 5,
    Card "Palace" Yellow 5,
--
    Card "Court of Miracles" Purple 2,
    Card "Dungeon" Purple 3,
    Card "Imperial Treasury" Purple 4,
    Card "Observatory" Purple 5,
    Card "Wishing Well" Purple 5,
    Card "Laboratory" Purple 5,
    Card "Factory" Purple 5,
    Card "Cemetary" Purple 5,
    Card "Quarry" Purple 5,
    Card "Library" Purple 6,
    Card "Great Wall" Purple 6,
    Card "University" Purple 6,
    Card "Magic School" Purple 6,
    Card "Dracoport" Purple 6
    ]


instance Colorful Card where
    colorful (Card name color cost) = (name `withStyle` colorStyle color) <> " (" <> (cost `showStyle` (bold<>yellow)) <> ")"

-- Every player has a role, a hand (that others cannot see), some cards on the table, and some gold.
data PlayerData = PlayerData {
    _role :: Maybe Role,
    _hand :: [Card],
    _table :: [Card],
    _gold :: Int
}

$(makeLenses ''PlayerData)

data GameData = GameData {
    _players :: M.Map Player PlayerData,
    _firstFinished :: Maybe Player
}

$(makeLenses ''GameData)

type Game = StateT GameData Play 


every :: (Bounded a, Enum a) => [a]
every = [minBound .. maxBound]

-- Setup the initial state for the game
gameSetup :: Play GameData
gameSetup = do 
    pnames <- allPlayers
    let pdata = PlayerData Nothing [] [] 0
    let players = M.fromList (zip pnames (repeat pdata))
    return $ GameData players Nothing

--The game is over when any player has 8 cards on the table
isGameOver :: Game Bool
isGameOver = anyOf (players.traverse.table.to length) (== 8) <$> get

-- Rules for computing the score at the end of the game
computeScore :: Player -> Game Int
computeScore player = do

    -- Which buildings do we have ?
    buildings <- use (players . ix player . table)

    -- Total cost we paid to build our city
    let baseCost = sum (cost <$> buildings)

    -- We are eligible for the color bonus if we have cards of all existing colors
    let hasAllColors = flip elem (cardColor <$> buildings) `all` (every :: [Color])
    let colorBonus = if hasAllColors then 3 else 0

    -- Completion bonus of 4 if we were the first player to finish the city
    isFirstFinished <- (Just player ==) <$> use firstFinished
    let finishBonus = if isFirstFinished then 4 else (if length buildings >= 8 then 2 else 0)

    let specialEffects = 0
    -- Final score
    return $ baseCost + colorBonus + finishBonus + specialEffects


displayScores :: Game ()
displayScores = do
    ps <- allPlayers
    scores <- sequence (map computeScore ps)
    forM_ ((reverse.sort) (zip scores ps)) $ \(s,p) -> 
        announce ("player " <> colorful p <> " has " <> cshow s <> " points.")


-- A single game turn
turn :: Game ()
turn = do

    -- first, check if the game is over, and if so, display scores
    done <- isGameOver
    when done $ displayScores >> raise "Game has ended"




        

game :: Play ()
game = do
    forPlayerNumber (4,7)

    initial <- gameSetup
    forever turn `evalStateT` initial



