{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module HTGL.Color
    ( Style
    , Colored
    , Colorful
    , Styling
    , style
    , bold, thin
    , black, blue, green, cyan, red, magenta, yellow, white
    , coloredLike
    , chunks
    , cshow
    , colorful
    , styling
    , withStyle
    ) where

import Control.Arrow (first)
import Control.Monad.Free
import Data.Monoid
import Data.String
import Data.Text
import Data.Functor.Classes

data Color8 = Black | Blue | Green | Cyan | Red | Magenta | Yellow | White
    deriving (Show,Eq,Ord)

data Style = Style { _color :: Last Color8, _bold :: Last Bool }
    deriving (Show)

instance Semigroup Style where
    Style ca ba <> Style cb bb = Style (ca <> cb) (ba <> bb)

instance Monoid Style where
    mempty = Style mempty mempty

style :: Style -> Colored s -> Colored s
style s (Colored t) = Colored (Free (Coloring [(s, t)]))

color :: Color8 -> Style
color c = Style (Last (Just c)) (Last Nothing)

bold = (Style (Last Nothing) (Last (Just True)))
thin = (Style (Last Nothing) (Last (Just False)))

black = color Black
blue = color Blue
green = color Green
cyan = color Cyan
red = color Red
magenta = color Magenta
yellow = color Yellow
white = color White

newtype Coloring t = Coloring [(Style, t)]
    deriving (Show, Functor)

newtype Colored s = Colored { unColored :: (Free Coloring s) }
    deriving (Show, Functor, Applicative, Monad)

instance Show1 Coloring where
    liftShowsPrec sp lp p (Coloring t) = ("Coloring "++) . liftShowsPrec f1 f2 p t where
        f1 = liftShowsPrec sp lp 
        f2 = liftShowList sp lp

instance Semigroup (Colored s) where
    -- not sure if this is smart or not
    -- Colored (Free (Coloring as)) <> Colored (Free (Coloring bs)) = Colored (Free (Coloring (as ++ bs)))
    Colored a <> Colored b = Colored (Free (Coloring [(mempty,a),(mempty,b)]))

instance Monoid (Colored s) where
    mempty = Colored (Free (Coloring []))

withStyle :: s -> Style -> Colored s
withStyle s style = Colored (Free (Coloring [(style,Pure s)]))

coloredLike :: Colored s -> Colored s -> Colored s
coloredLike s (Colored (Pure _)) = s
coloredLike s (Colored (Free (Coloring []))) = s
coloredLike (Colored s) (Colored (Free (Coloring ((st,_):_)))) = Colored (Free (Coloring [(st,s)]))


chunks :: Colored s -> [(Style, s)]
chunks (Colored (Pure s)) = [(mempty, s)]
chunks (Colored (Free (Coloring cs))) = [ (s <> s', c) | (s,t) <- cs, (s',c) <- chunks (Colored t) ]

instance IsString s => IsString (Colored s) where
    fromString = pure . fromString

class Colorful s where
    colorful :: s -> Colored Text

class Styling s where
    styling :: s -> Style

instance (Styling s, Show s) => Colorful s where
    colorful s = fromString (show s) `withStyle` styling s

instance Colorful (Colored Text) where
    colorful = id

cshow :: Show s => s -> Colored Text
cshow = fromString . show
