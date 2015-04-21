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

-- extern int micreate_volume(const char *filename, int number_of_dimensions,
--                            midimhandle_t dimensions[],
--                            mitype_t volume_type,
--                            miclass_t volume_class,
--                            mivolumeprops_t create_props,
--                            mihandle_t *volume);
{#fun micreate_volume{ `String', `Int',
                       allocaDims- `[Ptr ()]' peekDims*,
                       toCInt `MincMiType',
                       toCInt `MincMiClass',
                       id `Ptr ()',
                       alloca- `Ptr ()' peek* } -> `Int' #}

-- extern int micreate_volume_image(mihandle_t volume);
{#fun micreate_volume_image{ id `Ptr ()' } -> `Int' #}

-- FIXME How do we make a mivolumeprops_t?

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

-- extern int miget_slice_scaling_flag(mihandle_t volume, miboolean_t *slice_scaling_flag);
-- FIXME Might be nicer to use a Bool marshalling thing instead of a CInt (miboolean_t is just int).
{#fun miget_slice_scaling_flag{ id `Ptr ()', alloca- `CInt' peek* } -> `Int' #}

-- extern int miset_slice_scaling_flag(mihandle_t volume, miboolean_t slice_scaling_flag);
{#fun miset_slice_scaling_flag{ id `Ptr ()', `CInt' } -> `Int' #}

-- extern int miget_volume_dimensions(mihandle_t volume, midimclass_t dimclass, midimattr_t attr,
--                                    miorder_t order, int array_length,
--                                    midimhandle_t dimensions[]);
{#fun miget_volume_dimensions{ id `Ptr ()', toCInt `MincDimClass', toCUInt `MincDimAttribute',
                               toCInt `MincDimOrder', toCInt `Int', allocaDims- `[Ptr ()]' peekDims* } -> `Int' #}

-- int miget_dimension_sizes(const midimhandle_t dimensions[], misize_t array_length,
--                                 misize_t sizes[]);
{#fun miget_dimension_sizes{ withArray* `[Ptr ()]', toCULLong `Int', allocaDimSizes- `[Int]' peekDimSizes* } -> `Int' #}


-- int miget_dimension_separations(const midimhandle_t dimensions[],
--                                 mivoxel_order_t voxel_order,
--                                 misize_t array_length,
--                                 double separations[]);
{#fun miget_dimension_separations{ withArray* `[Ptr ()]', toCInt `MincVoxelOrder', toCULLong `Int',
                                   allocaSeparations- `[CDouble]' peekSeparations* } -> `Int' #}


-- int miget_dimension_starts(const midimhandle_t dimensions[], mivoxel_order_t voxel_order,
--                            misize_t array_length, double starts[]);
{#fun miget_dimension_starts{ withArray* `[Ptr ()]', toCInt `MincVoxelOrder', toCULLong `Int',
                              allocaStarts- `[CDouble]' peekStarts* } -> `Int' #}

-- int miget_dimension_name(midimhandle_t dimension, char **name_ptr);
{#fun miget_dimension_name{ id `Ptr ()', alloca- `CString' peek*} -> `Int' #}

-- withArrayInts :: Storable a => [a] -> (Ptr a -> IO b) -> IO b
withArrayInts x = withArray (map toCULLong x)

-- int miget_real_value_hyperslab(mihandle_t volume,
--                                mitype_t buffer_data_type,
--                                const misize_t start[],
--                                const misize_t count[],
--                                void *buffer);
{#fun miget_real_value_hyperslab{ id `Ptr ()',
                                  toCInt `MincType',
                                  withArrayInts* `[Int]',
                                  withArrayInts* `[Int]',
                                  id `Ptr ()' } -> `Int' #}

-- int miget_data_type(mihandle_t vol, mitype_t *volume_data_type);
{#fun miget_data_type{ id `Ptr ()', alloca- `MincType' peekMincType* } -> `Int' #}

-- int miget_hyperslab_size(mitype_t volume_data_type,
--                          int n_dimensions,
--                          const hsize_t count[],
--                          misize_t *size_ptr);
{#fun miget_hyperslab_size{ toCInt `MincType',
                            toCInt `Int',
                            withArrayInts* `[Int]',
                            alloca- `CULLong' peekIntConv* } -> `Int' #}

-- int miget_slice_dimension_count(mihandle_t volume, midimclass_t dimclass,
--                                 midimattr_t attr, int *number_of_dimensions);
{#fun miget_slice_dimension_count{ id `Ptr ()', toCInt `MincDimClass',
                                   toCUInt `MincDimAttribute', alloca- `Int' peekIntConv* } -> `Int' #}

-- /** Create a volume property list.  The new list will be returned in the
--  * \a props parameter.    When the program is finished
--  * using the property list it should call  mifree_volume_props() to free the
--  * memory associated with the list.
--  * \param props A pointer to the returned volume properties handle.
--  * \ingroup mi2VPrp
--  */
-- int minew_volume_props(mivolumeprops_t *props);
-- FIXME Automatically handle the call to mifree_volume_props.
{#fun minew_volume_props{ alloca- `Ptr ()' peek* } -> `Int' #}

-- int mifree_volume_props(mivolumeprops_t props);
-- FIXME Test
{#fun mifree_volume_props{ id `Ptr ()' } -> `Int' #}

{-

TODO


/** Get a copy of the volume property list.  When the program is finished
 * using the property list it should call  mifree_volume_props() to free the
 * memory associated with the list.
 * \param volume A volume handle
 * \param props A pointer to the returned volume properties handle.
 * \ingroup mi2VPrp
 */
int miget_volume_props(mihandle_t vol, mivolumeprops_t *props);


/** Set multi-resolution properties.  The \a enable_flag determines
 * whether or not thumbnail images will be calculated at all.  The \a
 * depth parameter determines the lowest-resolution image that will be
 * available.  The full resolution image is considered to be image #0,
 * the half resolution image is image #1, the quarter-resolution image
 * is #2, etc. Therefore a \a depth value of 2 implies both the half
 * and quarter resolution thumbnails will be calculated and stored in
 * the file.
 * \param props A volume property list handle
 * \param enable_flag TRUE if multiresolution support should be enabled in
 * this file.
 * \param depth The maximum depth of multiresolution data
 * to support.
 * \ingroup mi2VPrp
 */
int miset_props_multi_resolution(mivolumeprops_t props, miboolean_t enable_flag,
                                        int depth);



/** Get multi-resolution properties.  Returns the value of the \a enable_flag
 * and \a depth parameters.
 * \param props A volume property list handle
 * \param enable_flag Pointer to a boolean which will be set to TRUE if
 * multiresolution has been enabled.
 * \param depth Pointer to a integer which will contain the maximum resolution
 * depth enabled if multiresolution is enabled.
 * \ingroup mi2VPrp
 */
int miget_props_multi_resolution(mivolumeprops_t props, miboolean_t *enable_flag,
                                        int *depth);


/** Select a different resolution from a multi-resolution image.
 * \ingroup mi2VPrp
 */
int miselect_resolution(mihandle_t volume, int depth);


/** Compute or recompute all resolution groups.
 *
 * \ingroup mi2VPrp
 */
int miflush_from_resolution(mihandle_t volume, int depth);

/** Set compression type for a volume property list
 * Note that enabling compression will automatically
 * enable blocking with default parameters.
 * \param props A volume properties list
 * \param compression_type The type of compression to use (MI_COMPRESS_NONE
 * or MI_COMPRESS_ZLIB)
 * \ingroup mi2VPrp
 */
int miset_props_compression_type(mivolumeprops_t props, micompression_t compression_type);


/** Get compression type for a volume property list
 * \param props A volume property list handle
 * \param compression_type A pointer to a variable to which the current
 * compression type will be assigned.
 * \ingroup mi2VPrp
 */
int miget_props_compression_type(mivolumeprops_t props, micompression_t *compression_type);


/** Set zlib compression properties for a volume list.  The \a zlib_level
 * parameter may range from 1 to 9, where higher numbers request that the
 * library attempt to use more memory (and possibly processing power) to
 * achieve the highest possible compression ratio.
 *
 * \param props A volume property list handle
 * \param zlib_level An integer specifying the desired compression level.
 * \ingroup mi2VPrp
 */
int miset_props_zlib_compression(mivolumeprops_t props, int zlib_level);


/** Get zlib compression properties from a volume property list.
 * \param props A volume property list handle
 * \param zlib_level Pointer to an integer variable that will receive the
 * current compression level.
 * \ingroup mi2VPrp
 */
int miget_props_zlib_compression(mivolumeprops_t props, int *zlib_level);


/** Set blocking structure properties for the volume
 * \param props A volume property list handle
 * \param edge_count
 * \param edge_lengths
 * \ingroup mi2VPrp
 */
int miset_props_blocking(mivolumeprops_t props, int edge_count, const int *edge_lengths);


/** Get blocking structure properties for the volume
 * \param props The properties structure from which to get the information
 * \param edge_count Returns the number of edges (dimensions) in a block
 * \param edge_lengths The lengths of the edges
 * \param max_lengths The number of elements of the edge_lengths array
 * \ingroup mi2VPrp
 */
int miget_props_blocking(mivolumeprops_t props, int *edge_count, int *edge_lengths,
                                int max_lengths);


/** Set properties for uniform/nonuniform record dimension
 * \ingroup mi2VPrp
 */
int miset_props_record(mivolumeprops_t props, misize_t record_length, char *record_name);


/** Set the template volume flag
 * \ingroup mi2VPrp
 */
int miset_props_template(mivolumeprops_t props, int template_flag);

/** \defgroup mi2Slice SLICE/VOLUME SCALE FUNCTIONS */
/**
 * This function sets \a slice_max to the maximum real value of
 * voxels in the slice containing the coordinates \a start_positions.
 * The \a array_length may be less than or equal to the number of dimensions
 * in the volume, extra coordinates will be ignored.  Specifying too few
 * coordinates will trigger an error.
 * Coordinates must always be specified in raw file order.
 * \ingroup mi2Slice
 */
int miget_slice_max(mihandle_t volume,
                           const misize_t start_positions[],
                           size_t array_length, double *slice_max);

/**
 * This function sets minimum real value of
 * values in the slice containing the coordinates \a start_positions.
 * The \a array_length may be less than or equal to the number of dimensions
 * in the volume, extra coordinates will be ignored.  Specifying too few
 * coordinates will trigger an error.
 * Coordinates must always be specified in raw file order.
 * \ingroup mi2Slice
 */
int miset_slice_max(mihandle_t volume,
                           const misize_t start_positions[],
                           size_t array_length, double slice_max);


/**
 * This function sets \a slice_min to the minimum real value of
 * voxels in the slice containing the coordinates \a start_positions.
 * The \a array_length may be less than or equal to the number of dimensions
 * in the volume, extra coordinates will be ignored.  Specifying too few
 * coordinates will trigger an error.
 * Coordinates must always be specified in raw file order.
 * \ingroup mi2Slice
 */
int miget_slice_min(mihandle_t volume,
                           const misize_t start_positions[],
                           size_t array_length, double *slice_min);


/**
 * This function sets minimum real value of
 * values in the slice containing the coordinates \a start_positions.
 * The \a array_length may be less than or equal to the number of dimensions
 * in the volume, extra coordinates will be ignored.  Specifying too few
 * coordinates will trigger an error.
 * Coordinates must always be specified in raw file order.
 * \ingroup mi2Slice
 */
int miset_slice_min(mihandle_t volume,
                           const misize_t start_positions[],
                           size_t array_length, double slice_min);


/**
 * This function gets both the minimum and
 * maximum real value of voxels in the slice containing the coordinates
 * \a start_positions.  The \a array_length may be less than or equal to
 * the number of dimensions in the volume, extra coordinates will be
 * ignored.  Specifying too few coordinates will trigger an error.
 * Coordinates must always be specified in raw file order.
 * \ingroup mi2Slice
 */
int miget_slice_range(mihandle_t volume,
                             const misize_t start_positions[],
                             size_t array_length, double *slice_max,
                             double *slice_min);


/**
 * This function the minimum and maximum real value of voxels in the
 * slice containing the coordinates \a start_positions.  The \a
 * array_length may be less than or equal to the number of dimensions in
 * the volume, extra coordinates will be ignored.  Specifying too few
 * coordinates will trigger an error.  Coordinates must always be
 * specified in raw file order.
 * \ingroup mi2Slice
 */
int miset_slice_range(mihandle_t volume,
                             const misize_t start_positions[],
                             size_t array_length, double slice_max,
                             double slice_min);

/**
 * This function returns the maximum real value of
 * voxels in the entire \a volume.  If per-slice scaling is enabled, this
 * function will return an error.
 * \ingroup mi2Slice
 */
int miget_volume_max(mihandle_t volume, double *slice_max);


/**
 * This function sets the maximum real value of
 * voxels in the entire \a volume.  If per-slice scaling is enabled, this
 * function will return an error.
 * \ingroup mi2Slice
 */
int miset_volume_max(mihandle_t volume, double slice_max);


/**
 * This function returns the minimum real value of
 * voxels in the entire \a volume.  If per-slice scaling is enabled, this
 * function will return an error.
 * \ingroup mi2Slice
 */
int miget_volume_min(mihandle_t volume, double *slice_min);


/**
 * This function sets the minimum real value of
 * voxels in the entire \a volume.  If per-slice scaling is enabled, this
 * function will return an error.
 * \ingroup mi2Slice
 */
int miset_volume_min(mihandle_t volume, double slice_min);


/**
 * This function retrieves the maximum and minimum real values of
 * voxels in the entire \a volume.  If per-slice scaling is enabled, this
 * function will return an error.
 * \ingroup mi2Slice
 */
int miget_volume_range(mihandle_t volume, double *volume_max,
                              double *volume_min);

/**
 * This function sets the maximum and minimum real values of
 * voxels in the entire \a volume.  If per-slice scaling is enabled, this
 * function will return an error.
 * \ingroup mi2Slice
 */
int miset_volume_range(mihandle_t volume, double volume_max,
                              double volume_min);


/** \defgroup mi2Hyper HYPERSLAB FUNCTIONS */

/** Calculates and returns the number of bytes required to store the
 * hyperslab specified by the \a n_dimensions and the
 * \a count parameters, using hdf type id
 * \ingroup mi2Hyper
 */
void miget_hyperslab_size_hdf(hid_t hdf_type_id, int n_dimensions,
                                const hsize_t count[],
                                misize_t *size_ptr);


/** Reads the real values in the volume from the interval min through
 *  max, mapped to the maximum representable range for the requested
 *  data type. Float types is mapped to 0.0 1.0
 * \ingroup mi2Hyper
 */
int miget_hyperslab_normalized(mihandle_t volume,
                                      mitype_t buffer_data_type,
                                      const misize_t start[],
                                      const misize_t count[],
                                      double min,
                                      double max,
                                      void *buffer);

/** Writes the real values in the volume from the interval min through
 *  max, mapped to the maximum representable range for the requested
 *  data type. Float types is mapped to 0.0 1.0
 * \ingroup mi2Hyper
 */
int miset_hyperslab_normalized(mihandle_t volume,
                                      mitype_t buffer_data_type,
                                      const misize_t start[],
                                      const misize_t count[],
                                      double min,
                                      double max,
                                      void *buffer);

/** Get a hyperslab from the file,
 * converting voxel values into real values
 * \ingroup mi2Hyper
 */
int miget_hyperslab_with_icv(mihandle_t volume,
                                    mitype_t buffer_data_type,
                                    const misize_t start[],
                                    const misize_t count[],
                                    void *buffer);

/** Write a hyperslab to the file, converting real values into voxel values
 * \ingroup mi2Hyper
 */
int miset_hyperslab_with_icv(mihandle_t volume,
                                    mitype_t buffer_data_type,
                                    const misize_t start[],
                                    const misize_t count[],
                                    void *buffer);

/** Write a hyperslab to the file from the preallocated buffer,
 *  converting from the stored "voxel" data range to the desired
 * "real" (float or double) data range, same as miset_hyperslab_with_icv
 * \ingroup mi2Hyper
 */
int miset_real_value_hyperslab(mihandle_t volume,
                                      mitype_t buffer_data_type,
                                      const misize_t start[],
                                      const misize_t count[],
                                      void *buffer);

/** Read a hyperslab from the file into the preallocated buffer,
 * with no range conversions or normalization.  Type conversions will
 * be performed if necessary.
 * \ingroup mi2Hyper
 */
int miget_voxel_value_hyperslab(mihandle_t volume,
                                       mitype_t buffer_data_type,
                                       const misize_t start[],
                                       const misize_t count[],
                                       void *buffer);

/** Write a hyperslab to the file from the preallocated buffer,
 * with no range conversions or normalization.  Type conversions will
 * be performed if necessary.
 * \ingroup mi2Hyper
 */
int miset_voxel_value_hyperslab(mihandle_t volume,
                                       mitype_t buffer_data_type,
                                       const misize_t start[],
                                       const misize_t count[],
                                       void *buffer);


/** \defgroup mi2Cvt CONVERT FUNCTIONS */

/** Convert values between real (scaled) values and voxel (unscaled)
 * values.  The voxel value is the unscaled value, and corresponds to the
 * value actually stored in the file, whereas the "real" value is the
 * value at the given location after scaling has been applied.
 *
 * The \a coords parameter specifies the location at which the
 * conversion is performed.  This is needed because MINC supports
 * per-slice scaling, therefore a conversion performed at one location
 * may differ from that performed at another location.
 *
 * \param volume A volume handle
 * \param coords The position for which to perform the conversion.
 * \param ncoords The length of the \a coords array.
 * \param real_value The original real value, to be converted to voxel.
 * \param voxel_value_ptr A pointer to the converted voxel value.
 * \ingroup mi2Cvt
 */
int miconvert_real_to_voxel(mihandle_t volume,
                                   const misize_t coords[],
                                   size_t ncoords,
                                   double real_value,
                                   double *voxel_value_ptr);

/** Convert values between real (scaled) values and voxel (unscaled)
 * values.  The voxel value is the unscaled value, and corresponds to the
 * value actually stored in the file, whereas the "real" value is the
 * value at the given location after scaling has been applied.
 *
 * The \a coords parameter specifies the location at which the
 * conversion is performed.  This is needed because MINC supports
 * per-slice scaling, therefore a conversion performed at one location
 * may differ from that performed at another location.
 *
 * \param volume A volume handle
 * \param coords The position for which to perform the conversion.
 * \param ncoords The length of the \a coords array.
 * \param voxel_value The original voxel value, to be converted to real.
 * \param real_value_ptr A pointer to the converted real value.
 * \ingroup mi2Cvt
 */
int miconvert_voxel_to_real(mihandle_t volume,
                                   const misize_t coords[],
                                   int ncoords,
                                   double voxel_value,
                                   double *real_value_ptr);

/** Converts an N-dimensional spatial position in voxel coordinates into a
 * 3-dimensional spatial position in world coordinates.
 *
 * The returned world coordinate vector is in a standardized order, with
 * the X position first (at index 0), followed by the Y and Z coordinates.
 * The voxel coordinate vector is in the native order appropriate to the
 * file.
 *
 * \ingroup mi2Cvt
 */
int miconvert_voxel_to_world(mihandle_t volume,
                                    const double voxel[],
                                    double world[]);

/** Converts a 3-dimensional spatial position in world coordinates into a
 * N-dimensional spatial position in voxel coordinates.
 *
 * The input world coordinate vector is in a standardized order, with
 * the X position first (at index 0), followed by the Y and Z coordinates.
 * The voxel coordinate vector is in the native order appropriate to the
 * file.
 *
 * \ingroup mi2Cvt
 */
int miconvert_world_to_voxel(mihandle_t volume,
                                    const double world[],
                                    double voxel[]);

/** This function retrieves the real values of a position in the
 *  MINC volume.  The "real" value is the value at the given location
 *  after scaling has been applied.
 *
 * \param volume A volume handle
 * \param coords The voxel position to retrieve
 * \param ndims The number of values in the \a coords array
 * \param value_ptr Pointer to a double variable to hold the returned value.
 *
 * \ingroup mi2Cvt
 */
int miget_real_value(mihandle_t volume,
                            const misize_t coords[],
                            int ndims,
                            double *value_ptr);

/** This function sets the  real value of a position in the MINC
 *  volume. The "real" value is the value at the given location
 *  after scaling has been applied.
 *
 * \param volume A volume handle
 * \param coords The voxel position to retrieve
 * \param ndims The number of values in the \a coords array
 * \param value The value to save at this location.
 *
 * \ingroup mi2Cvt
 */
int miset_real_value(mihandle_t volume,
                            const misize_t coords[],
                            int ndims,
                            double value);


/** This function retrieves the voxel values of a position in the
 * MINC volume. The voxel value is the unscaled value, and corresponds
 * to the value actually stored in the file.
 *
 * \ingroup mi2Cvt
 */
int miget_voxel_value(mihandle_t volume,
                             const misize_t coords[],
                             int ndims,
                             double *voxel_ptr);


/** This function sets the voxel value of a position in the MINC
 * volume.  The voxel value is the unscaled value, and corresponds to the
 * value actually stored in the file.
 *
 * \ingroup mi2Cvt
 */
int miset_voxel_value(mihandle_t volume,
                             const misize_t coords[],
                             int ndims,
                             double voxel);

/** Get the absolute minimum and maximum values of a volume.
 *
 * \ingroup mi2Cvt
 */
int miget_volume_real_range(mihandle_t volume, double real_range[2]);

/**
 * This function sets the world coordinates of the point (0,0,0) in voxel
 * coordinates.  This changes the constant offset of the two coordinate
 * systems.
 *
 * \ingroup mi2Cvt
 */
int miset_world_origin(mihandle_t volume, double origin[MI2_3D]);

/* VALID functions */
/** This function gets the maximum valid value specific to the data
 *  type of the \a volume parameter.
 * \retval MI_ERROR on failure
 * \retval MI_NOERROR on success
 */
int miget_volume_valid_max(mihandle_t volume, double *valid_max);

/** This function sets the maximum valid value specific to the data
 *  type of the \a volume parameter.
 * \retval MI_ERROR on failure
 * \retval MI_NOERROR on success
 */
int miset_volume_valid_max(mihandle_t volume, double valid_max);

/** This function gets the minimum valid value specific to the data
 *  type of the \a volume parameter.
 * \retval MI_ERROR on failure
 * \retval MI_NOERROR on success
 */
int miget_volume_valid_min(mihandle_t volume, double *valid_min);

/** This function sets the minimum valid value specific to the data
 *  type of the \a volume parameter.
 * \retval MI_ERROR on failure
 * \retval MI_NOERROR on success
 */
int miset_volume_valid_min(mihandle_t volume, double valid_min);

/** This function gets the minimum and maximum valid value specific to the
 * data type of the \a volume parameter.
 * \retval MI_ERROR on failure
 * \retval MI_NOERROR on success
 */
int miget_volume_valid_range(mihandle_t volume, double *valid_max, double *valid_min);

/** This function sets the minimum and maximum valid value specific to the
 * data type of the \a volume parameter.
 * \retval MI_ERROR on failure
 * \retval MI_NOERROR on success
 */
int miset_volume_valid_range(mihandle_t volume, double valid_max, double valid_min);

/** \defgroup mi2Rec RECORD functions */
/** This method gets the name of the record dimension
 * TODO: set record name??
 * \ingroup mi2Rec
 */
int miget_record_name(mihandle_t volume, char **name);

/** This method gets the length (i.e., number of fields in the case of
 * uniform records and number of bytes for non_uniform ones) of the
 * record.
 * \ingroup mi2Rec
 */
int miget_record_length(mihandle_t volume, int *length);

/** This method returns the field name for the given field index.  Memory
 * for returned string is allocated on the heap and should be released using
 * mifree_name().
 * \ingroup mi2Rec
 */
int miget_record_field_name(mihandle_t volume, int index, char **name);

/** This method sets a field name for the volume record. The volume
 * must be of class "MI_CLASS_UNIFORM_RECORD".  The size of record
 * type will be increased if necessary to accomodate the new field.
 * \ingroup mi2Rec
 */
int miset_record_field_name(mihandle_t volume, int index,
                                   const char *name);

/** \ingroup mi2Label LABEL functions */

/**
 * This function associates a label name with an integer value for the given
 * volume. Functions which read and write voxel values will read/write
 * in integer values, and must call miget_label_name() to discover the
 * descriptive text string which corresponds to the integer value.
 * \ingroup mi2Label
 */
int midefine_label(mihandle_t volume, int value, const char *name);

/**
 * For a labelled volume, this function retrieves the text name
 * associated with a given integer value.
 *
 * The name pointer returned must be freed by calling mifree_name().
 * \ingroup mi2Label
*/
int miget_label_name(mihandle_t volume, int value, char **name);

/**
 * This function is the inverse of miget_label_name(). It is called to determine
 * what integer value, if any, corresponds to the given text string.
 * \ingroup mi2Label
*/
int miget_label_value(mihandle_t volume, const char *name, int *value);


/**
 * This function returns the number of defined labels, if any, or zero.
 * \ingroup mi2Label
*/
int miget_number_of_defined_labels(mihandle_t volume, int *number_of_labels);

/**
 * This function returns the label value associated with an index (0,1,...)
 * \ingroup mi2Label
*/
int miget_label_value_by_index(mihandle_t volume, int idx, int *value);

#ifdef __cplusplus
}
#endif /* __cplusplus defined */


-}


