### SETUP ###
# set directory to where the shared library is stored
setwd("~/ParallelForest/src/")
# source("test_grow_predict_01.r")



# load the shared libraries compiled in Fortran
dyn.load("~/ParallelForest/src/ParallelForest.dll")
is.loaded("ParallelForest")


source("../R/forest.r")
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



### DEFINE FUNCTIONS ###

#grow = function(xtrain, ytrain, min_node_obs, max_depth){
#
#    # get data size
#    n = nrow(xtrain)
#    p = ncol(xtrain)
#
#    # determine the maximum possible number of nodes with the given max depth for the 
#    # fitted tree, which determines the length of the padded array that the Fortran
#    # subroutine should return
#    TOP_NODE_NUM = 0
#    retlen = 2^(max_depth + 1 - TOP_NODE_NUM) - 1
#
#    # send to Fortran wrapper to grow tree
#    ret = .Fortran("grow_wrapper",
#        n=as.integer(n), p=as.integer(p),
#        xtrain=as.matrix(xtrain), ytrain=as.integer(ytrain),
#        min_node_obs=as.integer(min_node_obs), max_depth=as.integer(max_depth), 
#        retlen=as.integer(retlen),
#        tag_padded=integer(retlen),
#        tagparent_padded=integer(retlen),
#        tagleft_padded=integer(retlen),
#        tagright_padded=integer(retlen),
#        is_topnode_padded=integer(retlen),
#        depth_padded=integer(retlen),
#        majority_padded=integer(retlen),
#        has_subnodes_padded=integer(retlen),
#        splitvarnum_padded=integer(retlen),
#        splitvalue_padded=double(retlen),
#        numnodes=integer(1)
#        )
#
#    # unpad returned arrays and put everything into a list to return
#    fittedtree = list(
#        n=ret$n, p=ret$p,
#        xtrain=ret$xtrain, ytrain=ret$ytrain,
#        min_node_obs=ret$min_node_obs, max_depth=ret$max_depth,
#        tag=ret$tag_padded[1:ret$numnodes],
#        tagparent=ret$tagparent_padded[1:ret$numnodes],
#        tagleft=ret$tagleft_padded[1:ret$numnodes],
#        tagright=ret$tagright_padded[1:ret$numnodes],
#        is_topnode=ret$is_topnode_padded[1:ret$numnodes],
#        depth=ret$depth_padded[1:ret$numnodes],
#        majority=ret$majority_padded[1:ret$numnodes],
#        has_subnodes=ret$has_subnodes_padded[1:ret$numnodes],
#        splitvarnum=ret$splitvarnum_padded[1:ret$numnodes],
#        splitvalue=ret$splitvalue_padded[1:ret$numnodes],
#        numnodes=ret$numnodes
#        )
#
#    return(fittedtree)
#}
#
#
#
#predict = function(fittedtree, xnew){
#    n = as.integer(nrow(xnew))
#    p = as.integer(ncol(xnew))
#
#    if(p != fittedtree$p){
#        stop("New data has different number of variables than training data.")
#    }
#
#    retpred = .Fortran("predict_wrapper",
#        fittedtree$tag,
#        fittedtree$tagparent,
#        fittedtree$tagleft,
#        fittedtree$tagright,
#        fittedtree$is_topnode,
#        fittedtree$depth,
#        fittedtree$majority,
#        fittedtree$has_subnodes,
#        fittedtree$splitvarnum,
#        fittedtree$splitvalue,
#        fittedtree$numnodes,
#        n,
#        p,
#        xnew,
#        ynew_pred=integer(n)
#        )
#
#    return(retpred$ynew_pred)
#}



###### TESTS ON GROWING AND PREDICTING TREES #####
#### TEST 01 (ON EASY TO FIT DATA) ###
#
## fit data to tree, and make sure that predicted values on same data
## is the same as was inputted
#ft = grow(xtrain, ytrain, min_node_obs, max_depth)
#ft_samepred = predict(ft, xtrain)
#
## test failure conditions
#if(!all(ft$ytrain==ft_samepred)) {
#   stop("Test failed.")
#}

### TEST 02 (NEW DATA) ###

# same as above, except now with new data
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

#ynewhat = predict(ft, xnew)
#
## test failure conditions
#if(!all(ynew==ynewhat)) {
#   stop("Test failed.")
#}



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


