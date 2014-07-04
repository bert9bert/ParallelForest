#------------------------------------------------------------------------------
#   Test Fortran wrappers.
#   Copyright (C) 2014  Bertram Ieong
#   
#------------------------------------------------------------------------------


### SETUP ###
library(ParallelForest)

testsret = .Fortran("fortran_unit_tests_wrapper", exitflag=integer(1))

