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
	ftree = grow.tree(Y~., data=low_high_earners, min_node_obs=1, max_depth=20)
	ftree_samepred = predict(ftree, low_high_earners)

    if(sum(low_high_earners$Y==ftree_samepred)/nrow(low_high_earners) <= 0.95) {
       stop("Tree prediction on training data performs worse than 95%.")
    }
}


if(PERFORM_FOREST_TESTS){
	# test forest functions
	fforest = grow.forest(Y~., data=low_high_earners, min_node_obs=1000, max_depth=10,
	    numsamps=100000, numvars=5, numboots=5)
	fforest_samepred = predict(fforest, low_high_earners)

	if(sum(low_high_earners$Y==fforest_samepred)/nrow(low_high_earners) <= 0.70) {
       stop("Forest prediction on training data performs worse than 70%.")
    }
}
