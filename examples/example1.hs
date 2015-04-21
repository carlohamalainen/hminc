{-# LANGUAGE FlexibleContexts, QuasiQuotes #-}

module Main where

import Control.Applicative ((<$>))
import Control.Monad (forM_, when)

import Data.Word

import Data.Sequence (iterateN)

import Data.Array.Repa (computeP, traverse, Z(..), (:.)(..), Array, U, DIM0, DIM1, DIM2, DIM3, (!), sumAllP, Source(..))

import Data.Array.Repa.Eval (Target(..))

import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Eval as RE
import Data.Array.Repa.Repr.ForeignPtr (fromForeignPtr, F)

import Data.Array.Repa.IO.DevIL

import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2

import qualified Data.Vector.Unboxed as U

import Data.List (sort)

import Data.Minc
import Data.Minc.Raw
import Data.Minc.Raw.Base
import Data.Minc.Utils
import Data.Minc.Types

import System.IO (IOMode (..))

import Foreign.C.String (peekCString)
import Foreign.Marshal.Alloc (free, mallocBytes)
import Foreign.Marshal.Array
import Foreign.Ptr (castPtr, Ptr(..))
import Foreign.C.Types (CDouble(..))
import Foreign.Storable (sizeOf)

import qualified Foreign.ForeignPtr as FP

import qualified Data.HashSet as HS

import Text.Printf (printf)

type RepaRet3 a = Array F DIM3 a

-- | Take a slice in the first dimension of a 3D array.
slice0 :: (Monad m, Source r e, Target r e) => Array r DIM3 e -> Int -> m (Array r DIM2 e)
slice0 a i = computeP $ traverse a (\(e0 :. e1 :. _) -> (e0 :. e1)) (\f (Z :. j :. k) -> f (Z :. i :. j :. k))

-- | Take a slice in the second dimension of a 3D array.
slice1 :: (Monad m, Source r e, Target r e) => Array r DIM3 e -> Int -> m (Array r DIM2 e)
slice1 a j = computeP $ traverse a (\(e0 :. _ :. e2) -> (e0 :. e2)) (\f (Z :. i :. k) -> f (Z :. i :. j :. k))

-- | Take a slice in the third dimension of a 3D array.
slice2 :: (Monad m, Source r e, Target r e) => Array r DIM3 e -> Int -> m (Array r DIM2 e)
slice2 a k = computeP $ traverse a (\(e0 :. e1 :. _) -> (e0 :. e1)) (\f (Z :. i :. j) -> f (Z :. i :. j :. k))

writeSlice :: Source r Word8 => Array r DIM2 Word8 -> FilePath -> IO ()
writeSlice a f = do
    grey <- Grey <$> RE.copyP a :: IO Image
    runIL $ writeImage f grey

cross4 = [stencil2| 0 1 0
                    1 1 1
                    0 1 0 |]

dilate :: Source r Word8 => Array r DIM2 Word8 -> Array PC5 DIM2 Word8
dilate a = mapStencil2 (BoundConst 0) cross4 a

{-
dilateN n a =
    if n > 0
        then do a' <- dilate a
                -- a'' <- dilateN (n-1) a'
                return undefined
        else return a
-}


main :: IO ()
main = do
    let small = "/scratch/small.mnc"

    (_, volumePtr) <- miopen_volume small (mincIOMode ReadMode)
    print volumePtr

    dimensionCount <- runAccess "miget_volume_dimension_count " small $ chk $ miget_volume_dimension_count
                                                                                volumePtr
                                                                                Minc_Dim_Class_Any
                                                                                Minc_Dim_Attr_All
    print ("dimensionCount", dimensionCount)

    let Right dimensionCount' = dimensionCount

    Right dimensionPtrs <- runAccess "miget_volume_dimensions " small $ chk $ miget_volume_dimensions
                                                                                volumePtr
                                                                                Minc_Dim_Class_Any
                                                                                Minc_Dim_Attr_All
                                                                                Minc_Dim_Order_File
                                                                                dimensionCount'
    print $ take dimensionCount' dimensionPtrs

    Right dimensionSizes <- runAccess "miget_dimension_sizes " small $ chk $ miget_dimension_sizes
                                                                                dimensionPtrs
                                                                                dimensionCount'
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

    -- Check a few non-zero values:
    print $ take 40 $ filter (> 0) asArray

    -- Find the unique set of non-zero values:
    let values = sort . HS.toList . HS.fromList . filter (> 0) . map realToFrac $ asArray :: [Double]
    print ("values", values)

    -- Cast ??? to Repa?
    fptr <- FP.newForeignPtr_ d_double :: IO (FP.ForeignPtr CDouble)

    let
        [i, j, k] = take dimensionCount' dimensionSizes
        repa = fromForeignPtr (Z :. i :. j :. k) fptr :: RepaRet3 CDouble

    -- Look at a value:
    print $ repa ! (Z :. 0 :. 0 :. 0)

    let v0 = realToFrac (values !! 21) :: CDouble
    print ("v0", v0)

    let flabert :: CDouble -> Word8
        flabert x = if x > v0 - (0.05 :: CDouble) && x < v0 + (0.05 :: CDouble) then 255 else 0

    -- Extract to Word8....
    zzz <- computeP $ traverse repa id (\f (Z :. i :. j :. k) -> flabert $ f (Z :. i :. j :. k)) :: IO (Array U DIM3 Word8)

    -- This is probably overflowing!
    zzzsum <- sumAllP zzz
    print ("zzzsum", zzzsum)

    -- From the tutorial: https://wiki.haskell.org/Numeric_Haskell:_A_Repa_Tutorial#Building_shapes

    let nrI = dimensionSizes !! 0
        nrJ = dimensionSizes !! 1
        nrK = dimensionSizes !! 2

    forM_ [0..(nrJ - 1)] $ \j -> do
        -- The change-of-shape function ((\e0 :. _ :. e2) -> ...) has to be right, otherwise we get rubbish.
        -- I had blindly copied the example which had (\(e0 :. _ :. e2) -> (e0 :. e2)) and all the sums below
        -- ended up being zero.
        blah <- slice1 zzz j
        blahSum <- sumAllP blah :: IO Word8
        print (j, blahSum)

        -- Dump a greyscale image. The slice is a 'U' (Unboxed) but we need an 'F' (foreign) for the Grey function.
        grey <- Grey <$> RE.copyP blah :: IO Image
        runIL $ writeImage (printf "grey_%03d.png" j) grey

    -- Try out some stencils.
    oneSlice <- slice1 zzz 7

    let oneSliceDilated = dilate oneSlice
        -- oneSliceDilatedAFewTimes = dilateN 20 oneSlice

    oneSliceDilated' <- computeP oneSliceDilated :: IO (Array U DIM2 Word8)
    -- writeSlice oneSliceDilated' "oneSliceDilated.png"

    -- Volume properties? Need more tests.
    volProp <- runAccess "minew_volume_props" "(none)" $ chk minew_volume_props

    case volProp of
        Left err        -> print err
        Right volProp'  -> do x <- runAccess "mifree_volume_props" "(none)" $ chk $ mifree_volume_props volProp'
                              print x

    -- {#fun mifree_volume_props{ `()' } -> `Int' #}


    free d

    y <- miclose_volume volumePtr
    print y
