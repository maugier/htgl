{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections #-}

module HTGL.Interactive
    ( Time
    , Player(..)
    , Colorful
    , Block(..)
    , Event(..)
    , Announce
    , Interactive
    , send
    , say
    , withActive
    , event
    , allPlayers, activePlayer
    , within
    , runInteractive
    ) where

import Control.Monad.Identity
import Control.Monad.Reader
import Data.Text
import HTGL.Color
import Pipes.Core
import Pipes.Internal

{- This module describes the Interactive monad, which describes an 
 - exchange of messages between several players.
 -
 - Messages to the player are colorful text. Messages from the player
 - are plain text without formatting.
 -
 - There is an abstract notion of time; the interaction can attach a
 - timeout to any input request. Input events include the elapsed
 - time between the request and the reply.
 -
 - The monad also keeps track of an active player, used as the default
 - target for some interactions.
 -}


-- The game clock is in seconds
type Time = Integer

-- A player has a name and possibly a color
newtype Player = Player { playerName :: Text }
    deriving (Show, Read, Eq, Ord)

instance Colorful Player where
    colorful (Player name) = name `withStyle` (bold<>green)


-- How long should an input request block before the driver reports a timeout
data Block = Forever
           | Timeout Time

-- An input event provided by the driver. eventData should be Nothing if the
-- request timed out.
data Event = Event { eventTime :: Time, eventData :: (Maybe (Player, Text)) }

-- An outgoing message. If the Player is unspecified, the message is public,
-- otherwise it is a private message to that player.
type Announce = (Maybe Player, Colored Text)

-- A pipes proxy type for our interaction.
type IProxy = Proxy Block Event () Announce Identity


newtype Interactive a = Interactive { runInteractive :: ReaderT (Maybe Player, [Player]) (Proxy Block Event () Announce Identity) a }
    deriving (Functor, Applicative, Monad)


send :: Colorful a => Maybe Player -> a -> Interactive ()
send t = Interactive . lift . respond . (t,) . colorful


withActive :: Maybe Player -> Interactive a -> Interactive a
withActive p (Interactive m) = Interactive (local (\(_,ps) -> (p,ps)) m)

event :: Block -> Interactive Event
event = Interactive . lift . request


-- Retrieve the list of all the players
allPlayers :: Interactive [Player]
allPlayers = Interactive (snd<$>ask)

activePlayer :: Interactive (Maybe Player)
activePlayer = Interactive (fst<$>ask)

say :: Colorful a => a -> Interactive ()
say m = activePlayer >>= flip send m

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
               
