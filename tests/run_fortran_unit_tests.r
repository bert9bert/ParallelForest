#------------------------------------------------------------------------------
#   Test Fortran wrappers.
#   Copyright (C) 2014  Bertram Ieong
#   
#------------------------------------------------------------------------------


### SETUP ###
library(ParallelForest)

dyn.load('/home/bert/R/x86_64-pc-linux-gnu-library/3.0/ParallelForest/libs/ParallelForest.so')
testsret = .Fortran("fortran_unit_tests_wrapper", exitflag=integer(1))

