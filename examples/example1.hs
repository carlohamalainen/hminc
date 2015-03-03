module Main where

import Control.Monad (forM_, when)

import Data.Word

import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Eval as RE
import Data.Array.Repa.Repr.ForeignPtr (fromForeignPtr, F)

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

import Data.Minc.Boxed

-- import qualified Data.Vector.Generic.Base       as DVB
-- import qualified Data.Vector.Generic.Mutable    as DVM

-- instance DVB.Vector U.Vector CDouble where

-- instance U.Unbox CDouble

type RepaRet3 a = R.Array F R.DIM3 a

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
        repa = fromForeignPtr (R.Z R.:. i R.:. j R.:. k) fptr :: RepaRet3 CDouble

    -- Look at a value:
    print $ repa R.! (R.Z R.:. 0 R.:. 0 R.:. 0)

    -- I can't get this to work. Type errors. Repa doesn't have instances for various things
    -- so that CDouble can be 'unboxed'?
    -- let sum = R.sumAllP repa

    let v0 = realToFrac (values !! 0) :: CDouble

    let flabert :: CDouble -> Word8
        flabert x = if x > v0 - (0.05 :: CDouble) && x < v0 + (0.5 :: CDouble) then 1 else 0

    -- Extract to Word8....
    zzz <- R.computeP $ R.traverse repa id (\f (R.Z R.:. i R.:. j R.:. k) -> flabert $ f (R.Z R.:. i R.:. j R.:. k)) :: IO (R.Array R.U R.DIM3 Word8)

    -- From the tutorial: https://wiki.haskell.org/Numeric_Haskell:_A_Repa_Tutorial#Building_shapes
    -- Take a slice of one dimension. The resulting shape of the array changes
    -- ghci> computeP $ traverse x (\(e :. _) -> e) (\f (Z :. i :. j) -> f (Z :. i :. j :. 0)) :: IO (Array U DIM2 Int)
    -- AUnboxed ((Z :. 3) :. 3) (fromList [1,4,7,10,13,16,19,22,25])

    free d

    y <- miclose_volume volumePtr
    print y
