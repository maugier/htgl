{-|
Module      : HTGL.Play
Description : the Play monad

Implements the Play monad, used to describe game rules
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

module HTGL.Play 
    ( GameError
    , Play
    , Player
    , runPlay
    , announce
    , tell
    , raise
    , retry
    , allPlayers
    , forNumberOfPlayers
    , shuffle
    , eventFrom
    , tryRead
    , chooses
    ) where

import HTGL.Color
import qualified HTGL.Interactive as I
import HTGL.Interactive (Interactive, Player)
import Data.Text (Text, unpack)
import Control.Monad
import Control.Monad.Morph (MFunctor, hoist)
import Control.Monad.Random (RandT, MonadRandom, getRandomR, runRandT, liftRandT)
import Control.Monad.State (StateT, MonadState)
import Control.Monad.Trans (lift, MonadTrans)
import Control.Monad.Except (ExceptT, catchError, throwError, runExceptT)
import System.Random.Shuffle (shuffleM)
import System.Random (StdGen)

-- This is text and not colored text because we may want to test it for equality
type GameError = Text

newtype Play a = Play { runPlay' :: RandT StdGen (ExceptT GameError Interactive) a }
    deriving (Functor, Applicative, Monad, MonadRandom)


instance MFunctor (RandT g) where
    -- hoist :: Monad m => (forall a. m a -> n a) -> (RandT g m a -> RandT g n b)
    hoist f m = liftRandT (f . runRandT m)

liftInteractive :: Interactive a -> Play a
liftInteractive = Play . lift . lift

runPlay :: Play () -> StdGen -> Interactive ()
runPlay g gen = runExceptT (fst <$> runRandT (runPlay' g) gen) >>= either (I.announce . (`withStyle` red)) return

class (Monad m, MonadRandom m) => MonadPlay m where
    raise      :: Text -> m a
    tell       :: Player -> Colored Text -> m ()
    announce   :: Colored Text -> m ()
    allPlayers :: m [Player]
    retry      :: m a -> m a
    event      :: m (Player, Text)

instance MonadPlay Play where
    raise = Play . throwError
    tell = (liftInteractive .) . I.tell
    announce = liftInteractive . I.announce
    allPlayers = liftInteractive I.allPlayers
    retry (Play m) = Play (retry' m) where
        retry' m = (catchError m (\e -> (lift.lift.I.announce) e >> retry' m))
    event = liftInteractive (I.event I.Forever) >>= maybe (raise "Unexpected timeout") return . I.eventData

instance (MonadTrans t, MFunctor t, MonadPlay m, Monad (t m), MonadRandom (t m)) => MonadPlay (t m) where
    raise = lift . raise
    tell = (lift.) . tell
    announce = lift . announce
    allPlayers = lift allPlayers
    retry = hoist retry
    event = lift event

eventFrom :: (MonadPlay m) => Player -> m Text
eventFrom p = retry $ do
    (p', t) <-  event
    when (p' /= p) $ raise "Unexpected player"
    return t

tryRead :: (MonadPlay m, Read a) => Text -> m a
tryRead t = case readsPrec 0 (unpack t) of
    [(a,"")] -> return a
    _        -> raise "Invalid input"

chooses :: (MonadPlay m, Read a, Colorful a, Eq a) => Player -> [a] -> m a
chooses player available = retry $ do
    tell player $ "Pick one of " <> colorful available
    choice <- eventFrom player >>= tryRead
    (when.not) (choice `elem` available) $ raise "Invalid Choice"
    return choice

randomChoice :: MonadPlay m => [a] -> m a
randomChoice xs = (xs !!) <$> getRandomR (0, length xs - 1)

randomPlayer :: MonadPlay m => m Player
randomPlayer = allPlayers >>= randomChoice 

playerCount :: MonadPlay m => m Int
playerCount = length <$> allPlayers

shuffle :: MonadRandom m => [a] -> m [a]
shuffle = shuffleM

forNumberOfPlayers :: MonadPlay m => (Int, Int) -> m ()
forNumberOfPlayers (lo,hi) = do
    pc <- playerCount
    when ((pc < lo) || (pc > hi)) $ do
        announce ("This game is for " <> cshow lo <> " to " <> cshow hi <> " players.")
        raise "invalid player number"
