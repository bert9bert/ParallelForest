Package Description
===================
An R package for fitting random decision forests using parallel computing, with Fortran and OpenMP under the hood.

The basic features of this project are now functional (i.e. growing a forest, and predicting on the forest). The project has not been packaged yet, so to use it in its current state, it will need to be built from source. 

For now, on a Linux computer with R, gfortran, and GNU make installed:
1. Download and unarchive package contents, such as to `~/ParallelForest`.
2. Enter the `src` directory.
```bash
cd ~/ParallelForest/src/
```
3. Compile the Fortran shared library with gfortran.
```bash
make ParallelForest.dll
```
4. Put the following lines of code at the top of any R script that you want to run this package in.
```R
dyn.load("~/ParallelForest/src/ParallelForest.dll")
is.loaded("ParallelForest")

source("~/ParallelForest/R/forest.r")
source("~/ParallelForest/R/grow.forest.r")
source("~/ParallelForest/R/predict.forest.r")
```

The above is just temporary, and will be a lot cleaner once this package has been R packaged.

Once compiled and loaded, fit a forest using the syntax
```R
fittedforest = grow.forest(xtrain, ytrain, min_node_obs, max_depth, 
    numsamps, numvars, numboots)
```
where 
* `xtrain` is a double matrix of the predictors organized column-wise, 
* `ytrain` is an integer vector of binary outcomes coded to 0 or 1,
* `min_node_obs` is the minimum number of observations a node needs in order for it to be split,
* `max_depth` is the maximum depth of any contructed trees,
* `numsamps` is the number of bootstrap sampled observations for each tree in the forest,
* `numvars` is the number of various to randomly choose for each tree in the forest, and
* `numboots` is the number of trees to grow in the forest.