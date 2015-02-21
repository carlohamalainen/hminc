{-# LANGUAGE DeriveDataTypeable #-}

module Data.Minc.Types where

import Control.Exception
import Data.Typeable
import System.IO (IOMode (..))

data MincType = Minc_Byte           -- ^ 8-bit signed integer
              | Minc_Short          -- ^ 16-bit signed integer
              | Minc_Int            -- ^ 32-bit signed integer
              | Minc_Float          -- ^ 32-bit floating point
              | Minc_Double         -- ^ 64-bit floating point
              | Minc_String         -- ^ ASCII string
              | Minc_Ubyte          -- ^ 8-bit unsigned integer
              | Minc_Ushort         -- ^ 16-bit unsigned integer
              | Minc_Uint           -- ^ 32-bit unsigned integer
              | Minc_SComplex       -- ^ 16-bit signed integer complex
              | Minc_IComplex       -- ^ 32-bit signed integer complex
              | Minc_FComplex       -- ^ 32-bit floating point complex
              | Minc_DComplex       -- ^ 64-bit floating point complex
              | Minc_Unknown        -- ^ when the type is a record
            deriving (Eq, Show)

-- | Dimension attribute values.
data MincDimAttribute = Minc_Dim_Attr_All                       -- ^ MI_DIMATTR_ALL 0
                      | Minc_Dim_Attr_Regularly_Sampled         -- ^ MI_DIMATTR_REGULARLY_SAMPLED 0x1
                      | Minc_Dim_Attr_Not_Regularly_Sampled     -- ^ MI_DIMATTR_NOT_REGULARLY_SAMPLED 0x2

instance Enum MincDimAttribute where
    fromEnum Minc_Dim_Attr_All                      = 0
    fromEnum Minc_Dim_Attr_Regularly_Sampled        = 1
    fromEnum Minc_Dim_Attr_Not_Regularly_Sampled    = 2

    toEnum n = case n of
        0 -> Minc_Dim_Attr_All
        1 -> Minc_Dim_Attr_Regularly_Sampled
        2 -> Minc_Dim_Attr_Not_Regularly_Sampled
        _ -> throw (MincInvalidDimAttribute n)

data MincDimClass = Minc_Dim_Class_Any          -- ^ Don't care (or unknown)
                  | Minc_Dim_Class_Spatial      -- ^ Spatial dimensions (x, y, z)
                  | Minc_Dim_Class_Time         -- ^ Time dimension
                  | Minc_Dim_Class_SFrequency   -- ^ Spatial frequency dimensions
                  | Minc_Dim_Class_TFrequency   -- ^ Temporal frequency dimensions
                  | Minc_Dim_Class_User         -- ^ Arbitrary user-defined dimension
                  | Minc_Dim_Class_Record       -- ^ Record as dimension

instance Enum MincDimClass where
    fromEnum Minc_Dim_Class_Any         = 0
    fromEnum Minc_Dim_Class_Spatial     = 1
    fromEnum Minc_Dim_Class_Time        = 2
    fromEnum Minc_Dim_Class_SFrequency  = 3
    fromEnum Minc_Dim_Class_TFrequency  = 4
    fromEnum Minc_Dim_Class_User        = 5
    fromEnum Minc_Dim_Class_Record      = 6

    toEnum n = case n of
        0 -> Minc_Dim_Class_Any
        1 -> Minc_Dim_Class_Spatial
        2 -> Minc_Dim_Class_Time
        3 -> Minc_Dim_Class_SFrequency
        4 -> Minc_Dim_Class_TFrequency
        5 -> Minc_Dim_Class_User
        6 -> Minc_Dim_Class_Record
        _ -> throw (MincInvalidDimClass n)

instance Enum MincType where
    fromEnum Minc_Byte      = 1
    fromEnum Minc_Short     = 3
    fromEnum Minc_Int       = 4
    fromEnum Minc_Float     = 5
    fromEnum Minc_Double    = 6
    fromEnum Minc_String    = 7
    fromEnum Minc_Ubyte     = 100
    fromEnum Minc_Ushort    = 101
    fromEnum Minc_Uint      = 102
    fromEnum Minc_SComplex  = 1000
    fromEnum Minc_IComplex  = 1001
    fromEnum Minc_FComplex  = 1002
    fromEnum Minc_DComplex  = 1003
    fromEnum Minc_Unknown   = (-1)

    toEnum n = case n of
        1 -> Minc_Byte
        3 -> Minc_Short
        4 -> Minc_Int
        5 -> Minc_Float
        6 -> Minc_Double
        7 -> Minc_String
        100 -> Minc_Ubyte
        101 -> Minc_Ushort
        102 -> Minc_Uint
        1000 -> Minc_SComplex
        1001 -> Minc_IComplex
        1002 -> Minc_FComplex
        1003 -> Minc_DComplex
        (-1) -> Minc_Unknown
        _    -> throw (MincInvalidType n)

-- | Minc error types.
data MincError = MincError String Int String FilePath
               | MincInvalidArgs String
               | MincInvalidType          Int
               | MincInvalidDimClass      Int
               | MincInvalidDimAttribute  Int
               | MincInvalidDimOrder      Int
               deriving (Show, Typeable)

instance Exception MincError

mincIOMode :: IOMode -> Int
mincIOMode ReadMode  = 1 -- #define MI2_OPEN_READ 0x0001
mincIOMode WriteMode = 2 -- #define MI2_OPEN_RDWR 0x0002
mincIOMode _ = throw (MincInvalidArgs "IO mode")

-- | Minc dimension order.
data MincDimOrder = MincDimOrderFile        -- ^ File order.
                  | MincDimOrderApparent    -- ^ Apparent order.
                  deriving (Show, Typeable)

instance Enum MincDimOrder where
    fromEnum MincDimOrderFile       = 0
    fromEnum MincDimOrderApparent   = 1

    toEnum n = case n of
        0 -> MincDimOrderFile
        1 -> MincDimOrderApparent
        _    -> throw (MincInvalidDimOrder n)
