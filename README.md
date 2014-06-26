Package Description
===================
An R package for fitting random decision forests using parallel computing, with Fortran and OpenMP under the hood.

The basic features of this project are now functional (i.e. growing a forest, and predicting on the forest). The project has not been packaged yet, so to use it in its current state, it will need to be built from source. 

For now, on a Linux computer with R, gfortran, and GNU make installed:
* Download and unarchive package contents, such as to `~/ParallelForest`.
* Enter the `src` directory.
```bash
cd ~/ParallelForest/src/
```
* Compile the Fortran shared library with gfortran.
```bash
make ParallelForest.dll
```
* Put the following lines of code at the top of any R script that you want to run this package in.
```R
dyn.load("~/ParallelForest/src/ParallelForest.dll")
is.loaded("ParallelForest")

source("~/ParallelForest/R/forest.r")
source("~/ParallelForest/R/grow.forest.r")
source("~/ParallelForest/R/predict.forest.r")
```

This will be a lot cleaner once this package has been R packaged.