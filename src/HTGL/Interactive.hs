{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections #-}

module HTGL.Interactive where

import Control.Monad.Identity
import Control.Monad.Reader
import Data.Text
import HTGL.Color
import Pipes.Core
import Pipes.Internal

-- Game clock, in seconds
type Time = Integer

newtype Player = Player Text
    deriving (Show, Eq, Ord)

instance Colorful Player where
    colorful (Player name) = name `withStyle` (bold<>green)

data Block = Forever
           | Timeout Time

data Event = Event { eventTime :: Time, eventData :: (Maybe (Player, Text)) }

type Announce = (Maybe Player, Colored Text)

type IProxy a = Proxy Block Event () Announce Identity a

newtype Interactive a = Interactive { runInteractive :: ReaderT [Player] (Proxy Block Event () Announce Identity) a }
    deriving (Functor, Applicative, Monad)


class Playable a where
    readMove :: Text -> Maybe a 

announce :: Colorful a => a -> Interactive ()
announce = Interactive . lift . respond . (Nothing,) . colorful

tell :: Colorful a => Player -> a -> Interactive ()
tell p = Interactive . lift . respond . (Just p,) . colorful

event :: Block -> Interactive Event
event = Interactive . lift . request

allPlayers :: Interactive [Player]
allPlayers = Interactive ask

within :: Time -> a -> Interactive a -> Interactive a
within deadline timeout (Interactive (ReaderT action)) = Interactive (ReaderT (\r -> (trap deadline +>> action r))) where
    trap deadline (Timeout t2) | t2 < deadline = do   -- our deadline is longer, transparently pass answer
        Event elapsed result <- request (Timeout t2)
        next <- respond (Event elapsed result)
        trap (deadline - elapsed) next

    trap deadline other = do  -- our deadline is shorter, be ready to catch and abort
        Event elapsed result <- request (Timeout deadline)
        case result of 
            Nothing -> return timeout
            Just evt -> do
                next <- respond (Event elapsed result)
                trap (deadline - elapsed) next
               
