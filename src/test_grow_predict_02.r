# set directory to where the shared library is stored
setwd("~/ParallelDecisionTree/src/")

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
	min_node_obs=as.integer(2), max_depth=as.integer(10),
	tag=integer(tmp),tagparent=integer(tmp),tagleft=integer(tmp),tagright=integer(tmp),is_topnode=integer(tmp),
	depth=integer(tmp),majority=integer(tmp),has_subnodes=integer(tmp),splitvarnum=integer(tmp),splitvalue=double(tmp))

#if(!all(ret$ytrain==ret$ytesthat)) {
#   stop("Test failed.")
#}
