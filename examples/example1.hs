module Main where

import Data.Minc
import Data.Minc.Raw
import Data.Minc.Raw.Base
import Data.Minc.Utils
import Data.Minc.Types

import System.IO (IOMode (..))

main :: IO ()
main = do
    let small = "/scratch/small.mnc"

    (_, volumePtr) <- miopen_volume small (mincIOMode ReadMode)
    print volumePtr

    dimensionCount <- runAccess "foo" small $ chk $ miget_volume_dimension_count volumePtr Minc_Dim_Class_Any Minc_Dim_Attr_All
    print ("dimensionCount", dimensionCount)

    let Right dimensionCount' = dimensionCount

    foo <- runAccess "bar" small $ chk $ miget_volume_dimensions volumePtr Minc_Dim_Class_Any Minc_Dim_Attr_All Minc_Dim_Order_File dimensionCount'
    print foo

    y <- miclose_volume volumePtr
    print y
