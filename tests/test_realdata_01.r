#------------------------------------------------------------------------------
#   Test on real data.
#   Copyright (C) 2014
#------------------------------------------------------------------------------

library(ParallelForest)

PERFORM_TREE_TESTS = FALSE
PERFORM_FOREST_TESTS = TRUE
N_TESTS = 10

### SETUP ###
# Load real dataset
data(low_high_earners)


if(PERFORM_TREE_TESTS){
	# test tree functions
	ptm = proc.time()  # timing
	ftree = grow.tree(Y~., data=low_high_earners, min_node_obs=1, max_depth=20)
	print(proc.time() - ptm)  # timing
	ptm = proc.time()  # timing
	ftree_samepred = predict(ftree, low_high_earners)
	print(proc.time() - ptm)  # timing

    if(sum(low_high_earners$Y==ftree_samepred)/nrow(low_high_earners) <= 0.95) {
       stop("Tree prediction on training data performs worse than 95%.")
    }
}


test_realdata_forest = function() {
  ptm = proc.time()  # timing
  fforest = grow.forest(Y~., data=low_high_earners, min_node_obs=1000, max_depth=10,
                        numsamps=100000, numvars=5, numboots=5)
  print(proc.time() - ptm)  # timing
  ptm = proc.time()  # timing
  fforest_samepred = predict(fforest, low_high_earners)
  print(proc.time() - ptm)  # timing
  
  acc = sum(low_high_earners$Y==fforest_samepred)/nrow(low_high_earners)
  return(acc)
}

if(PERFORM_FOREST_TESTS){
  acc_vec = sapply(X=1:N_TESTS, FUN=function(dummy_input) test_realdata_forest())
  acc_avg = mean(acc_vec)
	if(acc_avg< 0.65) {
       stop("Forest prediction on training data performs worse than threshold.")
    }
}
