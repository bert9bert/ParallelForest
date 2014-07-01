#------------------------------------------------------------------------------
#   Test Fortran wrappers.
#   Copyright (C) 2014  Bertram Ieong
#------------------------------------------------------------------------------


PERFORM_TREE_TESTS = FALSE
PERFORM_FOREST_TESTS = TRUE


### SETUP ###
# set directory to where the shared library is stored
setwd("~/ParallelForest/tests/")
# source("test_fakedata_01.r")


# load the shared libraries compiled in Fortran
dyn.load("../src/ParallelForest.so")
is.loaded("ParallelForest")

source("../R/forest.r")
source("../R/tree.r")
source("../R/grow.tree.r")
source("../R/predict.tree.r")
source("../R/grow.forest.r")
source("../R/predict.forest.r")

### PREPARE FAKE DATA DATASET ####
# load training data
df = read.csv("../data/easy_2var_data.csv")

# create testing data
xnew = matrix(c(
        0.06,  0.03,
        0.05,  0.02,
        0.05,  0.05,
        0.01,  0.03,
        0.09,  0.04,
        0.05, -1000,
        0.05,  1000,
       -1000,  0.04,
        1000,  0.03
        ), 
    nrow=9, ncol=2, byrow=TRUE
    )
xnew = as.data.frame(xnew)
colnames(xnew) = c("X1","X2")

ynew = c(1, 0, 1, 0, 1, 0, 1, 1, 1)



### FITTING PARAMETERS ###
min_node_obs = 1
max_depth = 10



##### TREES #####
if(PERFORM_TREE_TESTS){
    ftree = grow.tree(Y~X1+X2, data=df, min_node_obs=min_node_obs, max_depth=max_depth)
    ftree_samepred = predict(ftree, df)
    ftree_ynewhat = predict(ftree, xnew)

    if(!all(df$Y==ftree_samepred)) {
       stop("Tree prediction on training data is different than training data.")
    }

    if(!all(ynew==ftree_ynewhat)) {
       stop("Tree prediction on new testing data is different than expected.")
    }
}

##### TESTS ON GROWING AND PREDICTING FORESTS #####
if(PERFORM_FOREST_TESTS){
    numsamps=90
    numvars=1
    numboots=20

    ### TEST 01 (ON EASY TO FIT DATA) ###
    fforest = grow.forest(Y~X1+X2, data=df, min_node_obs=min_node_obs, max_depth=max_depth,
        numsamps=numsamps, numvars=numvars, numboots=numboots)
    fforest_samepred = predict(fforest, df)

    # test failure conditions
    if(sum(df$Y==fforest_samepred)/nrow(df) <= 0.85) {
       stop("Tree prediction on training data performs worse than 85%.")
    }

    ### TEST 02 (NEW DATA) ###

    # same as above, except now with new data
    fforest_ynewhat = predict(fforest, xnew)

    # test failure conditions
    if(sum(ynew==fforest_ynewhat)/nrow(df) <= 0.85) {
       stop("Tree prediction on training data performs worse than 85%.")
    }

}
