#------------------------------------------------------------------------------
#   Test Fortran wrappers.
#   Copyright (C) 2014
#   
#------------------------------------------------------------------------------

library(ParallelForest)

testsret = .Fortran("fortran_unit_tests_wrapper", exitflag=integer(1))
if(testsret!=0) stop("At least one of the Fortran unit tests failed.")

