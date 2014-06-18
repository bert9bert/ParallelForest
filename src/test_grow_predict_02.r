# set directory to where the shared library is stored
setwd("~/ParallelDecisionTree/src/")
# source("test_grow_predict_02.r")



# load the shared libraries compiled in Fortran
dyn.load("~/ParallelDecisionTree/src/grow_wrapper.dll")
is.loaded("grow_wrapper")

tmp = 25

df = read.csv("../data/easy_2var_data.csv")


# define data
n = 100
p = 2

datamat = as.matrix(df)

xtrain = datamat[,c(1,2)]
ytrain = datamat[,3]


# TODO: make sure that xtrain and xtest are DOUBLE matrices

# fit data to tree, and make sure that predicted values on same data
# is the same as was inputted
ret = .Fortran("grow_wrapper",
	n=as.integer(n), p=as.integer(p),
	xtrain=as.matrix(xtrain), ytrain=as.integer(ytrain),
	min_node_obs=as.integer(1), max_depth=as.integer(10),
	tag_padded=integer(tmp),tagparent_padded=integer(tmp),tagleft_padded=integer(tmp),tagright_padded=integer(tmp),is_topnode_padded=integer(tmp),
	depth_padded=integer(tmp),majority_padded=integer(tmp),has_subnodes_padded=integer(tmp),splitvarnum_padded=integer(tmp),splitvalue_padded=double(tmp),
	numnodes=integer(1))

#if(!all(ret$ytrain==ret$ytesthat)) {
#   stop("Test failed.")
#}
