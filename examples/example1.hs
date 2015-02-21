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

    x <- miopen_volume small (mincIOMode ReadMode)

    dimensionCount <- runAccess "foo" small $ chk $ miget_volume_dimension_count (snd x) Minc_Dim_Class_Any Minc_Dim_Attr_All

    {-
    self.ndims = ndims.value
    self.ndims_misize_t = misize_t(ndims.value)
    r = libminc.miget_volume_dimensions(
        self.volPointer, MI_DIMCLASS_ANY,
        MI_DIMATTR_ALL, MI_DIMORDER_APPARENT,
        ndims, self.dims)
    -}

    y <- miclose_volume (snd x)

    print x
    print y
    print dimensionCount

