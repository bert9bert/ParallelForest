# set directory to where the shared library is stored
setwd("~/ParallelDecisionTree/src/")
# source("test_grow_predict_02.r")



# load the shared libraries compiled in Fortran
dyn.load("~/ParallelDecisionTree/src/grow_wrapper.dll")
is.loaded("grow_wrapper")

retlen = 25

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
	min_node_obs=as.integer(1), max_depth=as.integer(10), retlen=as.integer(retlen),
	tag_padded=integer(retlen),tagparent_padded=integer(retlen),tagleft_padded=integer(retlen),tagright_padded=integer(retlen),is_topnode_padded=integer(retlen),
	depth_padded=integer(retlen),majority_padded=integer(retlen),has_subnodes_padded=integer(retlen),splitvarnum_padded=integer(retlen),splitvalue_padded=double(retlen),
	numnodes=integer(1))

fittedtree = list(
	n=ret$n, p=ret$p,
	xtrain=ret$xtrain, ytrain=ret$ytrain,
	min_node_obs=ret$min_node_obs, max_depth=ret$max_depth,
	tag=ret$tag_padded[1:ret$numnodes],
	tagparent=ret$tagparent_padded[1:ret$numnodes],
	tagleft=ret$tagleft_padded[1:ret$numnodes],
	tagright=ret$tagright_padded[1:ret$numnodes],
	is_topnode=ret$is_topnode_padded[1:ret$numnodes],
	depth=ret$depth_padded[1:ret$numnodes],
	majority=ret$majority_padded[1:ret$numnodes],
	has_subnodes=ret$has_subnodes_padded[1:ret$numnodes],
	splitvarnum=ret$splitvarnum_padded[1:ret$numnodes],
	splitvalue=ret$splitvalue_padded[1:ret$numnodes],
	numnodes=ret$numnodes
	)

#if(!all(ret$ytrain==ret$ytesthat)) {
#   stop("Test failed.")
#}
