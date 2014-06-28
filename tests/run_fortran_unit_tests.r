#------------------------------------------------------------------------------
#   Test Fortran wrappers.
#   Copyright (C) 2014  Bertram Ieong
#   
#------------------------------------------------------------------------------


### SETUP ###
# set directory to where the shared library is stored
setwd("../tests/")
# source("run_fortran_unit_tests.r")

# load the shared libraries compiled in Fortran
dyn.load("../src/ParallelForest.dll")
is.loaded("ParallelForest")

testsret = .Fortran("fortran_unit_tests_wrapper", exitflag=integer(1))

