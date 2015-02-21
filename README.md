# hminc

Haskell MINC library. Currently pre-alpha.

Heavily based on [hnetcdf](https://github.com/ian-ross/hnetcdf). In
particular, code in ```Data.Minc.Raw.Base```, ```Data.Minc.Types```,
and ```Data.Minc.Utils``` is *very* similar to the equivalent files
in hnetcdf.

Information about the MINC file format: http://www.bic.mni.mcgill.ca/ServicesSoftware/MINC

## Goals

* FFI bindings to the low-level C library.
* Higher-level read/write interface.
* Interface witih [Repa](https://hackage.haskell.org/package/repa) arrays.
