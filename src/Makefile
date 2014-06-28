#------------------------------------------------------------------------------
#   Make file for compiling in Linux.
#   Copyright (C) 2014  Bertram Ieong
#------------------------------------------------------------------------------


OBJECTS = utils.o random_utils.o tree_utils.o sort_utils.o impurity_measures.o classification.o forest_parallel.o testing_program.o
FFLAGS = -g -fbounds-check -fopenmp

.PHONY: test clean 

test: testing_program.exe
	./testing_program.exe

ParallelForest.dll: utils.f90 random_utils.f90 tree_utils.f90 sort_utils.f90 \
impurity_measures.f90 classification.f90 forest_parallel.f90 \
grow_wrapper.f90 predict_wrapper.f90 \
grow_forest_wrapper.f90 predict_forest_wrapper.f90
	R CMD SHLIB -o ParallelForest_R.dll \
utils.f90 random_utils.f90 tree_utils.f90 sort_utils.f90 \
impurity_measures.f90 classification.f90 forest_parallel.f90 \
grow_wrapper.f90 predict_wrapper.f90 \
grow_forest_wrapper.f90 predict_forest_wrapper.f90


testing_program.exe: $(OBJECTS)
	gfortran $(FFLAGS) $(OBJECTS) -o testing_program.exe

%.o : %.f90
	gfortran $(FFLAGS) -c  $< 

clean:
	rm -f *.o *.exe *.mod
