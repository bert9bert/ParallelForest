#------------------------------------------------------------------------------
#   Test Fortran wrappers.
#   Copyright (C) 2014  Bertram Ieong
#------------------------------------------------------------------------------


### SETUP ###
# set directory to where the shared library is stored
setwd("~/ParallelForest/src/")
# source("test_grow_predict_01.r")



# load the shared libraries compiled in Fortran
dyn.load("~/ParallelForest/src/ParallelForest.dll")
is.loaded("ParallelForest")

source("../R/forest.r")
source("../R/tree.r")
source("../R/grow.forest.r")
source("../R/predict.forest.r")

### PREPARE TEST DATASET ####
# load dataset
df = read.csv("../data/easy_2var_data.csv")

# prep dataset
datamat = as.matrix(df)

xtrain = datamat[,c(1,2)]
ytrain = datamat[,3]


### FITTING PARAMETERS ###
min_node_obs = 1
max_depth = 10



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

ynew = c(1, 0, 1, 0, 1, 0, 1, 1, 1)


##### TESTS ON GROWING AND PREDICTING FORESTS #####
### TEST 01 (ON EASY TO FIT DATA) ###

# fit data to tree, and make sure that predicted values on same data
# is the same as was inputted
numsamps=90
numvars=1
numboots=20

fforest = grow.forest(xtrain, ytrain, min_node_obs, max_depth,
    numsamps, numvars, numboots)
fforest_samepred = predict(fforest, xtrain)

# test failure conditions
if(!all(ytrain==fforest_samepred)) {
   stop("Test failed.")
}



### TEST 02 (NEW DATA) ###

# same as above, except now with new data
forest_ynewhat = predict(fforest, xnew)

# test failure conditions
if(!all(ynew==forest_ynewhat)) {
   stop("Test failed.")
}


