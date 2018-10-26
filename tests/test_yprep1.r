#------------------------------------------------------------------------------
#   Test that the functions to prepare the dependent variable to send and
#   receive to the Fortran programs work.
#   Copyright (C) 2014  Bertram Ieong
#------------------------------------------------------------------------------

library(ParallelForest)


## define various dependent vectors to check, some should work and some should fail
yint01  = as.integer(c(1,1,1,0,0,1))  # should work
yint56  = as.integer(c(6,6,6,5,5,6))  # should work
yint7   = as.integer(c(7,7,7,7,7,7))  # should fail
yint345 = as.integer(c(5,3,4,3,3,3))  # should fail

ydbl01  = as.double(yint01)  # should work
ydbl56  = as.double(yint56)  # should work
ydbl7   = as.double(yint7)   # should fail
ydbl345 = as.double(yint345) # should fail

ydec2    = c(1.1,1.1,1.1,0.2,0.2,1.1)              # should work
ydec2alt = c(6.1,6.1,6.1,5.2,5.2,6.1)              # should work
ydec1    = rep(7.7, 6)                             # should fail
ydec3    = c(5.5,3.3,4.4,3.3,3.3,3.3)              # should fail

ystr2 = c("bob","adam","adam","bob","adam","adam")     # should work
ystr1 = rep("bob",6)                                   # should fail
ystr3 = c("bob","adam","adam","bob","charles","adam")  # should fail

yfac2 = as.factor(ystr2)  # should work
yfac1 = as.factor(ystr1)  # should fail
yfac3 = as.factor(ystr3)  # should fail
yfac2but3 = as.factor(c("bob","adam","adam","bob","adam","adam",  # should work
    "charles","charles","bob","charles","charles","bob"))[7:12]

yord2 = as.ordered(ystr2)  # should work
yord1 = as.ordered(ystr1)  # should fail
yord3 = as.ordered(ystr3)  # should fail
yord2but3 = as.ordered(c("bob","adam","adam","bob","adam","adam",  # should work
    "charles","charles","bob","charles","charles","bob"))[7:12]



## make sure that vectors that should work do work,
## and that vectors that should fail do fail

# define testing functions
shouldwork.test = function(x) {
    retlist = ParallelForest:::prep.depvar.in(x)
    x.back = do.call(ParallelForest:::prep.depvar.out, retlist)
    if(!identical(x,x.back)) stop("Doesn't work, but should work!")
}

shouldfail.test = function(x) {

    trycatch.result = tryCatch(
        {
            retlist = ParallelForest:::prep.depvar.in(x)
            ret = -1
        }, 
        error = function(e) return(0))

    if(trycatch.result!=0) stop("Doesn't fail, but should fail!")
}

# apply the vectors to the appropriate testing function
shouldwork.vecs = list(yint01, yint56, 
    ydbl01, ydbl56, 
    ydec2, ydec2alt, 
    ystr2, 
    yfac2, yfac2but3, 
    yord2, yord2but3)
rettest.shouldwork = lapply(shouldwork.vecs, shouldwork.test)

shouldfail.vecs = list(yint7, yint345, 
    ydbl7, ydbl345, 
    ydec1, ydec3, 
    ystr1, ystr3, 
    yfac1, yfac3, 
    yord1, yord3)
rettest.shouldfail = lapply(shouldfail.vecs, shouldfail.test)

# all tests were successful if no errors were raised

