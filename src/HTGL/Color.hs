{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module HTGL.Color
    ( Style(..)
    , Colored
    , Colorful
    , style
    , bold, thin
    , black, blue, green, cyan, red, magenta, yellow, white
    , coloredLike
    , chunks
    , cshow
    , colorful
    , withStyle
    , showStyle
    ) where

import HTGL.Color.Internal

