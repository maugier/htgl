{-|
Module      : HTGL.Play
Description : the Play monad

Implements the Play monad, used to describe game rules
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module HTGL.Play 
    ( GameError
    , Play
    , Player
    , runPlay
    , announce
    , tell
    , raise
    , allPlayers
    , forPlayerNumber
    , shuffle
    ) where

import HTGL.Color
import qualified HTGL.Interactive as I
import HTGL.Interactive (Interactive, Player)
import Data.Text (Text)
import Control.Monad
import Control.Monad.Morph (MFunctor, hoist)
import Control.Monad.Random (RandT, MonadRandom, getRandomR)
import Control.Monad.State (StateT, MonadState)
import Control.Monad.Trans (lift, MonadTrans)
import Control.Monad.Except (ExceptT, catchError, throwError)
import System.Random.Shuffle (shuffleM)
import System.Random (StdGen)

type GameError = Colored Text

newtype Play a = Play { runPlay :: RandT StdGen (ExceptT GameError Interactive) a }
    deriving (Functor, Applicative, Monad, MonadRandom)

liftInteractive :: Interactive a -> Play a
liftInteractive = Play . lift . lift

class (Monad m, MonadRandom m) => MonadPlay m where
    raise      :: Colored Text -> m a
    tell       :: Player -> Colored Text -> m ()
    announce   :: Colored Text -> m ()
    allPlayers :: m [Player]
    retry      :: m a -> m a

instance MonadPlay Play where
    raise = Play . throwError
    tell = (liftInteractive .) . I.tell
    announce = liftInteractive . I.announce
    allPlayers = liftInteractive I.allPlayers
    retry (Play m) = Play (catchError m (\e -> (lift.lift.I.announce) e >> m))

instance (MonadTrans t, MFunctor t, MonadPlay m, Monad (t m), MonadRandom (t m)) => MonadPlay (t m) where
    raise = lift . raise
    tell = (lift.) . tell
    announce = lift . announce
    allPlayers = lift allPlayers
    retry = hoist retry


randomChoice :: MonadPlay m => [a] -> m a
randomChoice xs = (xs !!) <$> getRandomR (0, length xs - 1)

randomPlayer :: MonadPlay m => m Player
randomPlayer = allPlayers >>= randomChoice 

playerCount :: MonadPlay m => m Int
playerCount = length <$> allPlayers

shuffle :: MonadRandom m => [a] -> m [a]
shuffle = shuffleM

forPlayerNumber :: MonadPlay m => (Int, Int) -> m ()
forPlayerNumber (lo,hi) = do
    pc <- playerCount
    when ((pc < lo) || (pc > hi)) $
        raise ("This game is for " <> cshow lo <> " to " <> cshow hi <> " players.")
