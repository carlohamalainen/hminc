{-# LANGUAGE TypeFamilies, FlexibleInstances #-}

module Data.Minc.Utils where

import Data.Minc.Types

import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Either

import Foreign hiding (unsafePerformIO)
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (liftM)

peekIntConv :: (Storable a, Integral a, Integral b) => Ptr a -> IO b
peekIntConv = liftM fromIntegral . peek

peekMincType :: Ptr CInt -> IO MincType
peekMincType = liftM (toEnum . fromIntegral) . peek

allocaDims :: (Ptr (Ptr ()) -> IO b) -> IO b
allocaDims = allocaArray mincMaxDims

peekDims :: Ptr (Ptr ()) -> IO [Ptr ()]
peekDims = peekArray mincMaxDims

allocaDimSizes :: (Ptr CULLong -> IO b) -> IO b
allocaDimSizes = allocaArray mincMaxDims

peekDimSizes :: Ptr CULLong -> IO [Int]
peekDimSizes = liftM (map fromIntegral) . peekArray mincMaxDims

allocaSeparations :: (Ptr CDouble -> IO b) -> IO b
allocaSeparations = allocaArray mincMaxDims

peekSeparations :: Ptr CDouble -> IO [CDouble]
peekSeparations = peekArray mincMaxDims

-- FIXME allocaSeparations and peekSeparations are basically the
-- same as allocaStarts and peekStarts...

allocaStarts :: (Ptr CDouble -> IO b) -> IO b
allocaStarts = allocaArray mincMaxDims

peekStarts :: Ptr CDouble -> IO [CDouble]
peekStarts = peekArray mincMaxDims

-- TODO More here.
mincMaxDims :: Int
mincMaxDims = 5

mincMaxDimNameLen :: Int
mincMaxDimNameLen = 256

-- | Wrapper for IO actions that are calls to Minc library functions.
type MincIO a = IO (Either MincError a)

-- | Reader environment for running an action with a given name (the 'String')
-- referring to a file (the 'FilePath'). For example:
--
-- > (_, volumePtr) <- miopen_volume "foo.mnc" (mincIOMode ReadMode)
-- > mtype <- runAccess "miget_data_type" "foo.mnc" $ chk $ miget_data_type volumePtr
type Access a = ReaderT (String, FilePath) (EitherT MincError IO) a

runAccess :: String -> FilePath -> Access a -> MincIO a
runAccess f p = runEitherT . flip runReaderT (f, p)

class Checkable a where
    type OutType a :: *
    status :: a -> Int
    proj :: a -> OutType a

instance Checkable Int where
    type OutType Int = ()
    status s = s
    proj _ = ()

instance Checkable (Int, a) where
    type OutType (Int, a) = a
    status (s, _) = s
    proj (_, a) = a

instance Checkable (Int, a, b) where
    type OutType (Int, a, b) = (a, b)
    status (s, _, _) = s
    proj (_, a, b) = (a, b)

instance Checkable (Int, a, b, c) where
    type OutType (Int, a, b, c) = (a, b, c)
    status (s, _, _, _) = s
    proj (_, a, b, c) = (a, b, c)

instance Checkable (Int, a, b, c, d) where
    type OutType (Int, a, b, c, d) = (a, b, c, d)
    status (s, _, _, _, _) = s
    proj (_, a, b, c, d) = (a, b, c, d)

instance Checkable (Int, a, b, c, d, e) where
    type OutType (Int, a, b, c, d, e) = (a, b, c, d, e)
    status (s, _, _, _, _, _) = s
    proj (_, a, b, c, d, e) = (a, b, c, d, e)

-- | Run an IO action and check the return status. All Minc functions
-- return @-1@ on error (as opposed to @0@ in NetCDF). This function
-- and the "Checkable" typeclass is copied directly from hnetcdf.
chk :: Checkable a => IO a -> Access (OutType a)
chk act = do
    res <- lift $ liftIO $ act
    let st  = status res
        val = proj   res
    (f, p) <- ask
    lift $ if st == (-1)
      then left $ MincError f st "FIXME - look up error" p
      else right val
