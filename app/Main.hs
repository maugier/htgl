module Main where

{-# LANGUAGE OverloadedStrings #-}

import HTGL.Interactive
import HTGL.Game.Citadels
import HTGL.Color.ANSI
import HTGL.Run.Debug
import Data.Text (pack)
import Control.Lens
import qualified Data.Map as M

main = undefined

alice, bob, charlie :: Player
alice = Player (pack "alice")
bob = Player (pack "bob")
charlie = Player (pack "charlie")

testData :: GameData
testData = GameData ps alice Nothing deck where
    ps = M.fromList [ (alice, role .~ Just King $ startingPlayer)
                    , (bob, role .~ Just Wizard $ startingPlayer)
                    , (charlie, role .~ Just Thief $ startingPlayer)]
