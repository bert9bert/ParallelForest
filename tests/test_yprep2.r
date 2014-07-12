#------------------------------------------------------------------------------
#   Test that grow and predict forest functions work on a variety of 
#   ways that the dependent variable can be put in.
#   Copyright (C) 2014  Bertram Ieong
#------------------------------------------------------------------------------

library(ParallelForest)


# Load fake dataset
data(easy_2var_data)

# Define different ways to represent the dependent variable
yint01 = as.integer(easy_2var_data$Y)

yint56 = rep(6, nrow(easy_2var_data))
yint56[easy_2var_data$Y==1] = 5
yint56 = as.integer(yint56)

ydec2 = yint01 + 3.5

ystr2 = rep("charles", nrow(easy_2var_data))
ystr2[easy_2var_data$Y==1] = "bob"

yfac2 = as.factor(ystr2)

yfac2but3 = as.factor(c("adam",ystr2))[-1]

yord2 = as.ordered(ystr2)

yord2but3 = as.ordered(c("adam",ystr2))[-1]

# define testing function and apply it #

testfn = function(depvar) {
    VERBOSE = FALSE

    test_data = easy_2var_data
    test_data$Y = depvar

    fforest = grow.forest(Y~., data=test_data, min_node_obs=1, max_depth=10,
        numsamps=100000, numvars=2, numboots=5)

    fforest_samepred = predict(fforest, test_data[-3])

    if(VERBOSE) {
        print(depvar)
        print(fforest_samepred)
    }

    if(!identical(depvar, fforest_samepred)) stop("Test failed.")
}

testvecs = list(yint01, yint56, 
    ydec2,
    ystr2, 
    yfac2, yfac2but3, 
    yord2, yord2but3)
rettest = lapply(testvecs, testfn)



# tests successful if no errors raised
