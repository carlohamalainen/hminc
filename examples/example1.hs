module Main where

import Control.Monad (forM_, when)

import Data.Minc
import Data.Minc.Raw
import Data.Minc.Raw.Base
import Data.Minc.Utils
import Data.Minc.Types

import System.IO (IOMode (..))

import Foreign.C.String (peekCString)
import Foreign.Marshal.Alloc (free, mallocBytes)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (castPtr, Ptr(..))
import Foreign.C.Types (CDouble(..))
import Foreign.Storable (sizeOf)

main :: IO ()
main = do
    let small = "/scratch/small.mnc"

    (_, volumePtr) <- miopen_volume small (mincIOMode ReadMode)
    print volumePtr

    dimensionCount <- runAccess "miget_volume_dimension_count " small $ chk $ miget_volume_dimension_count volumePtr Minc_Dim_Class_Any Minc_Dim_Attr_All
    print ("dimensionCount", dimensionCount)

    let Right dimensionCount' = dimensionCount

    Right dimensionPtrs <- runAccess "miget_volume_dimensions " small $ chk $ miget_volume_dimensions
                                                                                volumePtr
                                                                                Minc_Dim_Class_Any
                                                                                Minc_Dim_Attr_All
                                                                                Minc_Dim_Order_File
                                                                                dimensionCount'
    print $ take dimensionCount' dimensionPtrs

    Right dimensionSizes <- runAccess "miget_dimension_sizes " small $ chk $ miget_dimension_sizes dimensionPtrs dimensionCount'
    print $ take dimensionCount' dimensionSizes

    Right separations <- runAccess "miget_dimension_separations" small $ chk $ miget_dimension_separations
                                                                                dimensionPtrs
                                                                                Minc_Voxel_Order_File
                                                                                dimensionCount'
    print $ take dimensionCount' separations

    Right starts <- runAccess "miget_dimension_starts" small $ chk $ miget_dimension_starts
                                                                        dimensionPtrs
                                                                        Minc_Voxel_Order_File
                                                                        dimensionCount'
    print $ take dimensionCount' starts

    forM_ [0..(dimensionCount'-1)] $ \dimIdx -> do
        Right z <- runAccess "miget_dimension_name" small $ chk $ miget_dimension_name (dimensionPtrs !! dimIdx)
        name <- peekCString z
        print $ ("miget_dimension_name", dimIdx, name)
        free z -- FIXME We need to free this ourselves?

    Right mtype <- runAccess "miget_data_type" small $ chk $ miget_data_type volumePtr
    print $ ("mtype", mtype)

    Right nrBytes <- runAccess "miget_hyperslab_size" small $ chk $ miget_hyperslab_size
                                                                        Minc_Double
                                                                        dimensionCount'
                                                                        (take dimensionCount' dimensionSizes)
    print $ ("nrBytes", nrBytes)

    d <- mallocBytes (fromIntegral nrBytes)

    blah <- miget_real_value_hyperslab
                volumePtr
                Minc_Double -- we want doubles to be returned
                [0, 0, 0]
                (take dimensionCount' dimensionSizes)
                d

    -- Manual here, we know that it will be a
    let d_double = castPtr d :: Ptr CDouble

    let nrVoxels = foldl (*) 1 $ take dimensionCount' dimensionSizes

    asArray <- peekArray nrVoxels d_double

    print $ take 40 $ filter (> 0) asArray
    -- forM_ [0..(nrVoxels - 1)] $ \idx -> when print $ (idx, asArray !! idx)

    free d

    y <- miclose_volume volumePtr
    print y
