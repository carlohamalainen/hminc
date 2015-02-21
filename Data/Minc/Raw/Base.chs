{-# LANGUAGE ForeignFunctionInterface #-}

module Data.Minc.Raw.Base where

import Data.Minc.Utils
import Data.Minc.Types

import Foreign hiding (unsafePerformIO)
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (liftM)

#include <minc2.h>

-- VOLUME FUNCTIONS

{-
extern int micreate_volume(const char *filename, int number_of_dimensions,
			   midimhandle_t dimensions[],
			   mitype_t volume_type,
			   miclass_t volume_class,
			   mivolumeprops_t create_props,
			   mihandle_t *volume);
extern int micreate_volume_image(mihandle_t volume);
-}

toCInt :: Enum a => a -> CInt
toCInt = toEnum . fromEnum

toCUInt :: Enum a => a -> CUInt
toCUInt = toEnum . fromEnum

toCULLong :: Enum a => a -> CULLong
toCULLong = toEnum . fromEnum

-- toCULongLong :: Enum a => a -> CULongLong
-- toCULongLong = toEnum . fromEnum

-- extern int miget_volume_dimension_count(mihandle_t volume, midimclass_t dimclass, midimattr_t attr, int *number_of_dimensions);
{#fun miget_volume_dimension_count{ id `Ptr ()', toCInt `MincDimClass', toCUInt `MincDimAttribute', alloca- `Int' peekIntConv* } -> `Int' #}

-- extern int miget_volume_voxel_count(mihandle_t volume, int *number_of_voxels);
{#fun miget_volume_voxel_count{ id `Ptr ()', alloca- `Int' peekIntConv* } -> `Int' #}

-- extern int miopen_volume(const char *filename, int mode, mihandle_t *volume);
{#fun miopen_volume{ `String', `Int', alloca- `Ptr ()' peek* } -> `Int' #}

-- extern int miclose_volume(mihandle_t volume);
{#fun miclose_volume{ id `Ptr ()' } -> `Int' #}

{-
extern int miget_slice_scaling_flag(mihandle_t volume, miboolean_t *slice_scaling_flag);
extern int miset_slice_scaling_flag(mihandle_t volume,
				    miboolean_t slice_scaling_flag);
-}


-- extern int miget_volume_dimensions(mihandle_t volume, midimclass_t dimclass, midimattr_t attr,
--                                    miorder_t order, int array_length,
--                                    midimhandle_t dimensions[]);
{#fun miget_volume_dimensions{ id `Ptr ()', toCInt `MincDimClass', toCUInt `MincDimAttribute',
                               toCInt `MincDimOrder', toCInt `Int', allocaDims- `[Ptr ()]' peekDims* } -> `Int' #}

-- int miget_dimension_sizes(const midimhandle_t dimensions[], misize_t array_length,
--                                 misize_t sizes[]);
{#fun miget_dimension_sizes{ withArray* `[Ptr ()]', toCULLong `Int', allocaDimSizes- `[Int]' peekDimSizes* } -> `Int' #}
