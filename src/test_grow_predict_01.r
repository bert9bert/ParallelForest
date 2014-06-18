# set directory to where the shared library is stored
setwd("~/ParallelDecisionTree/src/")

# load the shared libraries compiled in Fortran
dyn.load("~/ParallelDecisionTree/src/grow_predict_rwrapper.dll")
is.loaded("grow_predict_rwrapper")



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
ret = .Fortran("grow_predict_rwrapper",
	n=as.integer(n), p=as.integer(p),
	xtrain=as.matrix(xtrain), ytrain=as.integer(ytrain),
	xtest=as.matrix(xtrain),ytesthat=integer(n))

if(!all(ret$ytrain==ret$ytesthat)) {
   stop("Test failed.")
}

