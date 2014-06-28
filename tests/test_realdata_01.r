#------------------------------------------------------------------------------
#   Test on real data.
#   Copyright (C) 2014  Bertram Ieong
#------------------------------------------------------------------------------


### SETUP ###
# set directory to where the shared library is stored
setwd("~/ParallelForest/tests/")
# source("test_realdata_01.r")



# load the shared libraries compiled in Fortran
dyn.load("../src/ParallelForest.so")
is.loaded("ParallelForest")

source("../R/forest.r")
source("../R/tree.r")
source("../R/grow.forest.r")
source("../R/predict.forest.r")


load("../data/low_high_earners.rda")



# fit forest
fforest = grow.forest(Y~., data=low_high_earners, min_node_obs=10000, max_depth=10,
    numsamps=100000, numvars=5, numboots=5)

