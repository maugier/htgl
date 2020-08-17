{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{-| A dice game for HTGL
 -
 -
 -}

module HTGL.Game.Zanzibar where


import HTGL.Play
import HTGL.Color
import HTGL.Dice

import Control.Monad
import Data.List (sortBy, groupBy)
import Data.Ord (comparing)
import Data.Function (on)


score :: [Int] -> Int
score xs = case sortBy (comparing negate) xs
    of
        [4,2,1]    -> 10
        [1,1,1]    -> 7
        [x,1,1]    -> x
        [x,y,z]
         | x == y   && y == z   -> 3*x
         | x == y+1 && y == z+1 -> 2
        [2,2,1]    -> 0
        _          -> 1
        



chooseLeader :: [Player] -> Play Player
chooseLeader [] = raise "You can't play with 0 players"
chooseLeader [p] = do
    announce $ colorful p <> " starts the round."
    return p
chooseLeader ps = do
    announce $ colorful ps <> " roll to decide who will start."
    rolls <- forM ps $ \p -> do
        announce $ colorful p <> " rolls..."
        (p,) <$> roll d6
    let winners = map fst . head . groupBy ((==) `on` snd) . sortBy (comparing snd) $ rolls
    announce $ colorful winners <> " have the best rolls."
    chooseLeader winners


phase1 :: [Player] -> Play ()
phase1 = undefined



game :: Play ()
game = do

    players <- allPlayers
    leader <- chooseLeader players
    let turnOrder ps = if head ps == leader
                        then ps
                        else turnOrder (tail ps ++ [head ps])

    phase1 (turnOrder players)



 
     

