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
    , MonadPlay
    , Play
    , Player
    , runPlay
    , announce
    , tell
    , say
    , raise
    , retry
    , allPlayers
    , activePlayer
    , withActive
    , with
    , public
    , forNumberOfPlayers
    , shuffle
    , eventFrom
    , tryRead
    , choose
    , choosePlayer
    ) where

import HTGL.Color
import qualified HTGL.Interactive as I
import HTGL.Interactive (Interactive, Player)
import Data.Text (Text, pack, unpack)
import Control.Monad
import Control.Monad.Fail
import Control.Monad.Morph (MFunctor, hoist)
import Control.Monad.Random (RandT, MonadRandom, getRandomR, runRandT, liftRandT)
import Control.Monad.State (StateT, MonadState)
import Control.Monad.Trans (lift, MonadTrans)
import Control.Monad.Except (ExceptT, catchError, throwError, runExceptT)
import System.Random.Shuffle (shuffleM)
import System.Random (StdGen)

-- This is text and not colored text because we may want to test it for equality
type GameError = Text

newtype Play a = Play { runPlay' :: ExceptT GameError (RandT StdGen Interactive) a }
    deriving (Functor, Applicative, Monad, MonadRandom)


instance MFunctor (RandT g) where
    -- hoist :: Monad m => (forall a. m a -> n a) -> (RandT g m a -> RandT g n b)
    hoist f m = liftRandT (f . runRandT m)

liftInteractive :: Interactive a -> Play a
liftInteractive = Play . lift . lift

runPlay :: Play () -> StdGen -> Interactive ()
--runPlay g gen = runExceptT (fst <$> runRandT (runPlay' g) gen) >>= either (I.announce . (`withStyle` red)) return
runPlay g gen = runRNG . runErrors . runPlay' $ g where
    runErrors = (>>= either (lift . I.send Nothing . (`withStyle` red)) return) . runExceptT
    runRNG = (fst <$>) . (`runRandT` gen)

instance MonadFail Play where
    fail = raise . pack

class (Monad m, MonadRandom m) => MonadPlay m where
    raise        :: Text -> m a
    send         :: Maybe Player -> Colored Text -> m ()
    allPlayers   :: m [Player]
    activePlayer :: m (Maybe Player)
    withActive   :: Maybe Player -> m a -> m a
    retry        :: m a -> m a
    event        :: m (Player, Text)

instance MonadPlay Play where
    raise = Play . throwError
    send = (liftInteractive .) . I.send
    allPlayers = liftInteractive I.allPlayers
    activePlayer = liftInteractive I.activePlayer
    withActive p (Play m) = Play (hoist (hoist (I.withActive p)) m)
    retry (Play m) = Play (retry' m) where
        retry' m = (catchError m (\e -> (lift.lift.I.say) e >> retry' m))
    event = liftInteractive (I.event I.Forever) >>= maybe (raise "Unexpected timeout") return . I.eventData

instance (MonadTrans t, MFunctor t, MonadPlay m, Monad (t m), MonadRandom (t m)) => MonadPlay (t m) where
    raise = lift . raise
    send = (lift.) . send
    allPlayers = lift allPlayers
    activePlayer = lift activePlayer
    withActive p = hoist (withActive p)
    retry = hoist retry
    event = lift event

announce :: MonadPlay m => Colored Text -> m ()
announce = send Nothing

tell :: MonadPlay m => Player -> Colored Text -> m ()
tell = send . Just

say :: MonadPlay m => Colored Text -> m ()
say m = activePlayer >>= flip send m

with :: (MonadPlay m) => m a -> Player -> m a
with = flip (withActive . Just)

public :: (MonadPlay m) => m a -> m a
public = withActive Nothing

eventFrom :: (MonadPlay m) => Player -> m Text
eventFrom p = retry $ do
    (p', t) <-  event
    when (p' /= p) $ raise "Unexpected player"
    return t

tryRead :: (MonadPlay m, Read a) => Text -> m a
tryRead t = case readsPrec 0 (unpack t) of
    [(a,"")] -> return a
    _        -> raise "Invalid input"

choose :: (MonadPlay m, Read a, Colorful a, Eq a) => [a] -> m a
choose available = do
    player <- activePlayer
    case player of
        Nothing -> raise "Mo active player"
        Just player -> retry $ do
            say $ "Pick one of " <> colorful available
            choice <- eventFrom player >>= tryRead
            (when.not) (choice `elem` available) $ raise "Invalid Choice"
            return choice

choosePlayer :: (MonadPlay m) => m Player
choosePlayer = allPlayers >>= choose

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
