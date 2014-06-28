#------------------------------------------------------------------------------
#   Make file for compiling in Linux.
#   Copyright (C) 2014  Bertram Ieong
#------------------------------------------------------------------------------

.PHONY: clean 

ParallelForest.so: utils.f90 random_utils.f90 tree_utils.f90 sort_utils.f90 \
impurity_measures.f90 classification.f90 forest_parallel.f90 \
grow_wrapper.f90 predict_wrapper.f90 \
grow_forest_wrapper.f90 predict_forest_wrapper.f90
	R CMD SHLIB -o ParallelForest.so \
utils.f90 random_utils.f90 tree_utils.f90 sort_utils.f90 \
impurity_measures.f90 classification.f90 forest_parallel.f90 \
grow_wrapper.f90 predict_wrapper.f90 \
grow_forest_wrapper.f90 predict_forest_wrapper.f90 \
fortran_unit_tests_wrapper.f90

clean:
	rm -f *.o *.exe *.mod *.dll
