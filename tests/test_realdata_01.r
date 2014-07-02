#------------------------------------------------------------------------------
#   Test on real data.
#   Copyright (C) 2014  Bertram Ieong
#------------------------------------------------------------------------------


PERFORM_TREE_TESTS = TRUE
PERFORM_FOREST_TESTS = TRUE


### SETUP ###
# set directory to where the shared library is stored
setwd("~/ParallelForest/tests/")
# source("test_realdata_01.r")



# load the shared libraries compiled in Fortran
dyn.load("../src/ParallelForest.so")
is.loaded("ParallelForest")

source("../R/s4class_utils.r")
source("../R/forest.r")
source("../R/tree.r")
source("../R/grow.tree.r")
source("../R/predict.tree.r")
source("../R/grow.forest.r")
source("../R/predict.forest.r")

# Load real dataset
load("../data/low_high_earners.rda")


if(PERFORM_TREE_TESTS){
	# test tree functions
	ptm = proc.time()  # timing
	ftree = grow.tree(Y~., data=low_high_earners, min_node_obs=1, max_depth=20)
	print(proc.time() - ptm)  # timing
	ptm = proc.time()  # timing
	ftree_samepred = predict(ftree, low_high_earners)
	print(proc.time() - ptm)  # timing

    if(sum(low_high_earners$Y==ftree_samepred)/nrow(low_high_earners) <= 0.95) {
       stop("Tree prediction on training data performs worse than 95%.")
    }
}


if(PERFORM_FOREST_TESTS){
	# test forest functions
	ptm = proc.time()  # timing
	fforest = grow.forest(Y~., data=low_high_earners, min_node_obs=1000, max_depth=10,
	    numsamps=100000, numvars=5, numboots=5)
	print(proc.time() - ptm)  # timing
	ptm = proc.time()  # timing
	fforest_samepred = predict(fforest, low_high_earners)
	print(proc.time() - ptm)  # timing

	if(sum(low_high_earners$Y==fforest_samepred)/nrow(low_high_earners) <= 0.65) {
       stop("Forest prediction on training data performs worse than 70%.")
    }
}
