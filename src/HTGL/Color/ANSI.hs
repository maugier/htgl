module HTGL.Color.ANSI where

import Control.Monad (forM_)
import Data.Monoid (Last(..))
import HTGL.Color.Internal as H
import Data.Text (Text)
import Data.Text.IO (putStr)
import System.Console.ANSI as A (SGR(..), setSGR, ConsoleLayer(..), ColorIntensity(..), Color(..))

styleToSGR :: Style -> [SGR]
styleToSGR (Style (Last color) (Last bold)) = [SetColor Foreground (g bold) (f color)] where
    f (Just H.White) = A.White
    f (Just H.Red) = A.Red
    f (Just H.Green) = A.Green
    f (Just H.Yellow) = A.Yellow
    f (Just H.Blue) = A.Blue
    f (Just H.Cyan) = A.Cyan
    f (Just H.Magenta) = A.Magenta
    f (Just H.Black) = A.Black
    f Nothing = A.White

    g (Just True) = Vivid
    g _ = Dull

colorPutStrLn :: Colored Text -> IO ()
colorPutStrLn x = do
    forM_ (chunks x) $ \(style,text) -> do
        setSGR . styleToSGR $ style
        Data.Text.IO.putStr text 
    setSGR [Reset]
    putStrLn ""

cprint :: Colorful t => t -> IO ()
cprint = colorPutStrLn . colorful
    
