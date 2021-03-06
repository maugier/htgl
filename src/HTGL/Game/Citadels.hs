{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module HTGL.Game.Citadels where

import Control.Lens
import Control.Lens.TH
import Control.Monad
import Control.Monad.State (StateT, evalStateT, get)
import Data.String
import Data.Text (Text)
import Data.List (sort, reverse, (\\))
import qualified Data.Map as M
import HTGL.Play
import HTGL.Color

{- Citadelles, a card game by Bruno Faidutti -}


{- Contents of the game -}


-- Some game elements are associated with a color. Here are the possible colors.
data Color = Red
           | Blue
           | Green
           | Yellow
           | Purple
           deriving (Show,Eq,Ord,Enum,Bounded)

-- We associate those colors with a display style
colorStyle :: Color -> Style
colorStyle Red = bold <> red
colorStyle Blue = bold <> blue
colorStyle Green = bold <> green
colorStyle Yellow = bold <> yellow
colorStyle Purple = magenta

-- Render the color names as just the name with their appropriate style
instance Colorful Color where
    colorful color = color `showStyle` colorStyle color


-- We have 8 character cards. Order is important.
data Role = Assassin 
          | Thief 
          | Wizard
          | King
          | Merchant
          | Bishop
          | Architect
          | Condottiere
          deriving (Show, Read, Eq, Ord, Enum, Bounded)


-- Some role cards, but not all, bear a specific color.
roleColor :: Role -> Maybe Color
roleColor King = Just Yellow
roleColor Merchant = Just Green
roleColor Bishop = Just Blue
roleColor Condottiere = Just Red
roleColor _ = Nothing

-- render the role as their corresponding color if applicable, otherwise bold white
instance Colorful Role where
    colorful role = role `showStyle` maybe (bold<>white) colorStyle (roleColor role)

-- The game uses cards. Each card has a name, a color, and a cost indicated in gold pieces.
data Card = Card {
    name :: !Text,
    cardColor :: !Color,
    cost :: !Int
} deriving (Show, Eq, Ord)

-- Render a card, including name and cost in yellow
instance Colorful Card where
    colorful (Card name color cost) = (name `withStyle` colorStyle color) <> " (" <> (cost `showStyle` (bold<>yellow)) <> ")"


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

-- Every player has a role, a hand (that others cannot see), some cards on the table, and some gold.
data PlayerData = PlayerData {
    _role :: Maybe Role,
    _hand :: [Card],
    _table :: [Card],
    _gold :: Int
} deriving (Show)

$(makeLenses ''PlayerData)

-- A player initializes with no role, no cards and no gold.
startingPlayer = PlayerData Nothing [] [] 0

-- In addition to player data, the game also has a current turn leader (the king)
-- we also keep track of who finished a complete city first.
data GameData = GameData {
    _players :: M.Map Player PlayerData,
    _currentKing :: Player,
    _firstFinished :: Maybe Player,
    _drawPile :: [Card],
    _discardPile :: [Card],
    _assassinVictim :: Maybe Role,
    _thiefVictim :: Maybe Role
} deriving (Show)


$(makeLenses ''GameData)

-- Our game is a stateful game using GameData as th state
type Game = StateT GameData Play 


every :: (Bounded a, Enum a) => [a]
every = [minBound .. maxBound]

unwrap :: Text -> Maybe a -> Game a
unwrap reason = maybe (raise reason) return

reshuffleDiscard :: Game ()
reshuffleDiscard = do
    announce "Draw deck empty. Shuffling the discard pile."
    discarded <- use discardPile
    shuffled <- shuffle discarded
    drawPile %= (++ shuffled)
    discardPile .= []
    

drawCards :: Int -> Game [Card]
drawCards 0 = return []
drawCards n = do
    remaining <- use (drawPile . to length)
    when (remaining <= n) reshuffleDiscard
    drawPile %%= (\pile -> (take n pile, drop n pile))

discard :: [Card] -> Game ()
discard cards = discardPile %= (++cards)
            


{- Game setup, and victory conditions -}

-- We initialize the game data by creating a map of starting players,
-- and a shuffled deck of cards.
gameSetup :: Play GameData
gameSetup = do 
    pnames <- allPlayers
    let players = M.fromList ((,startingPlayer) <$> pnames)
    drawPile <- shuffle deck
    return $ GameData players (head pnames) Nothing drawPile [] Nothing Nothing

--The game is over when any player has 8 cards on the table at the end of a turn
isGameOver :: Game Bool
isGameOver = anyOf (players.traverse.table.to length) (== 8) <$> get

-- Rules for computing the score at the end of the game
computeScore :: Player -> Game Int
computeScore player = do

    -- Check all buildings that the player has on the table
    buildings <- use (players . ix player . table)

    -- The base cost of all these cards
    let baseCost = sum (cost <$> buildings)

    -- We are eligible for the color bonus if we have cards of all existing colors
    let hasAllColors = flip elem (cardColor <$> buildings) `all` (every :: [Color])
    let colorBonus = if hasAllColors then 3 else 0

    -- Completion bonus of 4 if we were the first player to finish the city
    isFirstFinished <- (Just player ==) <$> use firstFinished
    let finishBonus = if isFirstFinished then 4 else (if length buildings >= 8 then 2 else 0)

    -- Specials not yet implemented
    let specialEffects = 0

    -- Final score
    return $ baseCost + colorBonus + finishBonus + specialEffects

-- Display the scores to everyone
displayScores :: Game ()
displayScores = do
    ps <- allPlayers
    scores <- sequence (map computeScore ps)
    forM_ ((reverse.sort) (zip scores ps)) $ \(s,p) -> 
        announce ("player " <> colorful p <> " has " <> cshow s <> " points.")



{- First phase - Character selection -}

-- The playing order in the first phase is the table order, starting
-- with who is the current king
turnOrder :: Game [Player]
turnOrder = do
    ps <- allPlayers
    leader <- use currentKing
    return $ rotate ps leader where
        rotate (x:xs) l | x == l = (x:xs)
                        | otherwise = rotate (xs ++ [x]) l

-- let a player choose a role among those available, and pass the rest
pickRole :: [Role] -> Player -> Game [Role]
pickRole available p = (`with` p) $ do
    say "Choose your role for this turn."
    pick <- choose available
    say $ "You're now " <> colorful pick
    (players . ix p . role) .= Just pick
    return (available \\ [pick])
    
-- Phase 1 of the game - we shuffle the roles and assign them to players
phase1 :: Game ()
phase1 = do

    announce "Character choice phase starts."

    -- First, compute the turn order
    players <- turnOrder

    -- How many roles to exclude depends on the number of players
    let exclude = case length players of
                    4 -> 2
                    5 -> 1
                    6 -> 0
                    7 -> 0

    -- Some roles will be publicly excluded. The rest are secret.
    (excluded, roles') <- retry $ do
        (e,r) <- splitAt exclude <$> shuffle every
        -- If the king was among the excluded cards, retry the shuffle process
        when (King `elem` e) $ raise "The king cannot be along the openly removed roles"
        return (sort e, sort r)

    -- Everyone sees those excluded cards (if there are any)
    (when.not.null) excluded 
        (announce $ "Roles " <> colorful excluded <> " are not in play")

    -- secretly exclude 1 card
    let (excludedHidden, roles) = splitAt 1 roles'

    -- let the first 6 players chose their cards, pass to the next neighbor
    last <- foldM pickRole (sort roles) (take 6 players) 

    -- If there is a 7th player, let him chose between the last card and the secretly
    -- excluded card
    foldM pickRole (sort (last ++ excludedHidden)) (drop 6 players)

    return ()


whois :: Role -> Game (Maybe Player)
whois who = preuse ((players . itraversed <. (role . filtered (== Just who))) . withIndex . _1)

whois' :: Role -> Game Player
whois' = whois >=> unwrap "inexistant player"

-- Check if any player has a given role, and if so, reveal it to the table.
reveal :: Role -> Game (Maybe Player)
reveal who = do
    player <- whois who
    announce $ case player of
        Just someone -> colorful someone <> " is the " <> colorful who <> "!"
        Nothing -> "nobody is the " <> colorful who
    return player

    
data Action = GetGold | DrawCards | UseSpecial
    deriving (Show, Read, Eq, Ord)

instance Colorful Action where
    colorful = cshow

data WizAction = Exchange | Steal
    deriving (Show, Read, Eq, Ord)

instance Colorful WizAction where
    colorful Exchange = Exchange `showStyle` green
    colorful Steal    = Steal `showStyle` red

specialAction :: Role -> Game ()
specialAction Assassin = do
    say "Who do you want to kill ?"
    target <- choose (every \\ [Assassin])
    announce $ "The " <> colorful Assassin <> " kills the " <> colorful target <> "!"
    assassinVictim .= Just target

specialAction Thief = do
    say "Who do you want to steal from ?"
    target <- choose (every \\ [Assassin,Thief])
    announce $ "The " <> colorful Thief <> " steals from the " <> colorful target <> "!"
    thiefVictim .= Just target
specialAction Wizard = do
    Just p <- activePlayer
    say "Do you want to discard or exchange ?"
    act <- choose [Exchange,Steal]
    case act of
        Exchange -> do
            Just p <- activePlayer
            oldHand <- use (players . ix p . hand) 
            let amount = length oldHand
            discard oldHand
            newHand <- drawCards amount
            players . ix p . hand .= newHand


        Steal -> do
            target <- choosePlayer
            hand1 <- use (players . ix target . hand)
            hand2 <- use (players . ix p . hand)
            players . ix target . hand .= hand2
            players . ix p . hand .= hand1


phase2Turn :: Role -> Player -> Game ()
phase2Turn role player = do
    robbed <- use thiefVictim
    when (robbed == Just role) $ do
        thief <- whois' Thief
        money <- preuse (players . ix player . gold) >>= unwrap "missing player"
        players . ix player . gold .= 0
        players . ix thief . gold += money

        

phase2 :: Game ()
phase2 = do

    announce "Action phase starts!"

    forM_ every $ \role -> do
        killed <- use assassinVictim
        if (killed == Just role)
            then announce $ "The " <> colorful role <> " has been killed and skips their turn."
            else do
                player <- reveal role

                case player of
                    Nothing -> return ()
                    Just player -> phase2Turn role player
    
    announce "Action phase is over."

    

    

-- A single game turn
turn :: Game ()
turn = do

    -- first, check if the game is over, and if so, display scores
    done <- isGameOver
    when done $ displayScores >> raise "Game has ended"

    phase1
    phase2
    
    -- Reset the turn-specific data
    assassinVictim .= Nothing
    thiefVictim .= Nothing

        

game :: Play ()
game = do
    forNumberOfPlayers (4,7)

    initial <- gameSetup
    forever turn `evalStateT` initial



