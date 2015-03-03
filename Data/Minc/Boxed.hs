{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell, TypeFamilies, FlexibleInstances #-}

module Data.Minc.Boxed where

import Foreign.C.Types (CDouble(..))

import Data.Vector.Unboxed.Base (Unbox)
import Data.Vector.Unboxed.Deriving

cdoubleToDouble :: CDouble -> Double
cdoubleToDouble = undefined

doubleToCDouble :: Double -> CDouble
doubleToCDouble = undefined

derivingUnbox "CDouble"
    [t| CDouble -> Double |]
    [| cdoubleToDouble |]
    [| doubleToCDouble |]
