{-|
Module      : HTGL.Play
Description : the Play monad

Implements the Play monad, used to describe game rules
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HTGL.Play 
    ( GameError
    , Play
    , Player
    , runGame
    , invalid
    , currentPlayer
    , allPlayers
    , randomPlayer
    , nextPlayer
    ) where

import HTGL.Color
import HTGL.Interactive
import Data.Text
import Control.Monad.Random (RandT)
import Control.Monad.State (StateT)
import Control.Monad.Except (ExceptT, throwError)
import System.Random (StdGen)

type GameError = Text

newtype Play a = Play { runGame :: RandT StdGen (ExceptT GameError Interactive) a }
    deriving (Functor, Applicative, Monad)


invalid :: Text -> Play a
invalid = Play . throwError

currentPlayer :: Play Player
currentPlayer = undefined

allPlayers :: Play [Player]
allPlayers = undefined

randomPlayer :: Play Player
randomPlayer = undefined

nextPlayer :: Play ()
nextPlayer = undefined
