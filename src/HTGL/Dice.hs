{-# LANGUAGE RankNTypes #-}

module HTGL.Dice where

import Control.Applicative
import Control.Monad
import Control.Monad.Random
import HTGL.Play

newtype Dice = Dice { roll :: forall m. MonadPlay m => m Int }

d :: Int -> Dice
d n = Dice $ do
    r <- getRandomR (1,n)
    say $ "Rolling a d" <> cshow n <> "... [" <> cshow r <> "]"
    return r

d4 = d 4
d6 = d 6
d8 = d 8
d10 = d 10
d12 = d 12
d20 = d 20
d100 = d 100


instance Num Dice where
    (Dice a + Dice b) = Dice ((+) <$> a <*> b)
    (Dice a * Dice b) = Dice $ do
        r <- a
        sum <$> sequence (replicate r b)
    fromInteger n = Dice (return n)
