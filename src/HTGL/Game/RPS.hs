{-# LANGUAGE OverloadedStrings #-}

{-| Rock Paper Scissors for HTGL
 -
 -}

module HTGL.Game.RPS where

import HTGL.Play
import HTGL.Color

-- The type of allowed RPS moves.
-- deriving Show is required for 'showStyle'.
-- deriving Read is required for 'choose'.
data Move = Rock | Paper | Scissors
    deriving (Show,Read,Eq,Ord) 

-- Pretty colors for our 3 moves
moveStyle :: Move -> Style
moveStyle Rock = bold<>black
moveStyle Paper = bold<>white
moveStyle Scissors = bold<>blue

-- A simple victory function
beats :: Move -> Move -> Bool
Rock     `beats` Scissors = True
Scissors `beats` Paper    = True
Paper    `beats` Rock     = True
_        `beats` _        = False


-- We define how our moves will be rendered to text.
instance Colorful Move where
    colorful m = m `showStyle` moveStyle m

-- We can build our game rules out of small blocks in the Play monad.
-- This particular function lets the active player choose one of the three moves.
turn :: Play Move
turn = do
    say "it is your turn to play !" -- 'say' will send a private message to the active player
                                    -- the argument must be a Colorful Text

    choose [Rock,Paper,Scissors]    -- 'choose' will request a choice from the currently active player, and return it

-- The body of the game itself runs in the Play monad,
-- which provides a RNG, player enumeration, and interaction.
game :: Play ()
game = do

    forNumberOfPlayers (2,2) -- Convenient guard that will immediately stop the game
                             -- if the player count is not in the specified range.
                             --
    [player1,player2] <- allPlayers -- The game would stop with an exception if the match
                                    -- failed, but we just checked the player count.

    move1 <- turn `with` player1 -- `with` locally sets the active player.
    move2 <- turn `with` player2

    announce $ case (move1 `beats` move2, move2 `beats` move1) of -- announce sends a message to all players, regardless of who's active
                    (True,False) -> colorful player1 <> " wins !" -- Use (<>) from Semigroup to concatenate colorful text
                    (False,True) -> colorful player2 <> " wins !"
                    _            -> "it's a tie !"
    

