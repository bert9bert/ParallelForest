Package Description
===================
An R package for fitting random decision forests using parallel computing, with Fortran and OpenMP under the hood.

This package is still a work in progress, but the basic features of this project are now functional (i.e. growing a forest and predicting on the forest). To use it in its current experimental state, it will need to be built from source. This package will be submitted to CRAN once it reaches a stable point in its development.

To install this package from source on a Linux computer with R, R Dev, and gfortran installed:

1. Download and un-archive package contents, such as to `/randomdirectory/ParallelForest`.

2. Go to the directory one above the `ParallelForest` directory and build the R package. The following can be typed in a shell with by replacing `randomdirectory` with the directory that you un-archived `ParallelForest` into.
	```bash
	cd /randomdirectory/
	R CMD build ParallelForest
	```

3. Then open R, and install the package
	```R
	setwd("/randomdirectory/")
	install.packages("ParallelForest_1.0.0.tar.gz", repos=NULL)
	```

4. To use the package, load it as normal.
	```R
	library("ParallelForest")
	```

5. To uninstall the package, run
	```R
	remove.packages("ParallelForest")
	```


Once compiled and loaded, fit a forest using the syntax
```R
fittedforest = grow.forest(xtrain, ytrain, min_node_obs, max_depth, numsamps, numvars, numboots)
```
where 
* `xtrain` is a double matrix of the predictors organized column-wise, 
* `ytrain` is an integer vector of binary outcomes coded to 0 or 1,
* `min_node_obs` is the minimum number of observations a node needs in order for it to be split,
* `max_depth` is the maximum depth of any contructed trees,
* `numsamps` is the number of bootstrap sampled observations for each tree in the forest,
* `numvars` is the number of various to randomly choose for each tree in the forest, and
* `numboots` is the number of trees to grow in the forest.

Once a forest is grown, use it to predict outcomes on new data with the syntax
```R
ynewhat = predict(fittedforest, xnew)
```
where
* `fittedforest` is the outputted forest object from `grow.forest`, and
* `xnew` is a double matrix of new data to predict on.
