{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module HTGL.Run.Debug where

import HTGL.Play
import HTGL.Interactive
import HTGL.Color
import HTGL.Color.ANSI
import Data.Functor.Identity (runIdentity)
import Data.Text (pack, strip)
import Control.Monad
import Control.Monad.Random (runRandT)
import Control.Monad.Reader (runReaderT)
import System.Random (StdGen, newStdGen, mkStdGen)
import Text.Read (readMaybe)
import Pipes
import Pipes.Core
import Pipes.Internal

repeatRead :: Read a => IO a
repeatRead = getLine >>= maybe (putStrLn "invalid input." >> repeatRead) return . readMaybe 


displayConsole :: Announce -> Effect' IO ()
displayConsole (dest, message) = lift . colorPutStrLn $ line where
    line = case dest of
            Nothing -> "*** " <> message
            (Just player) -> colorful player <> ": " <> message

queryConsole :: Block -> Effect' IO Event
queryConsole Forever = lift $ do
    putStr "from: "
    name <- getLine
    putStr "msg: "
    command <- getLine
    return $ Event 0 (Just ((Player . strip . pack $ name),(strip . pack $ command)))


queryConsole _ = error "timeout undefined in debug"


debugGame :: Play() -> IO ()
debugGame = (newStdGen >>=) . debugGameWithGen

debugGameWithSeed :: Play () -> Int -> IO ()
debugGameWithSeed = (. mkStdGen) . debugGameWithGen


debugGameWithGen :: Play () -> StdGen -> IO ()
debugGameWithGen g gen = do

    putStr "Please enter a list of players:"
    players <- (Player . pack <$>) <$> repeatRead

    let g' = runPlay g gen
    let gameProxy = unsafeHoist (return . runIdentity) (runReaderT (runInteractive g') players)

    let process = (queryConsole >\\ gameProxy //> displayConsole)

    runEffect process


