OBJECTS = random_utils.o utils.o tree_utils.o sort_utils.o impurity_measures.o classification.o forest_parallel.o testing_program.o
FFLAGS = -g -fbounds-check

.PHONY: test clean 

test: testing_program.exe
	./testing_program.exe

grow_wrapper.dll: utils.f90 tree_utils.f90 sort_utils.f90 impurity_measures.f90 classification.f90 grow_wrapper.f90
	gfortran -fPIC -shared utils.f90 tree_utils.f90 sort_utils.f90 impurity_measures.f90 classification.f90 grow_wrapper.f90 -o grow_wrapper.dll

predict_wrapper.dll: utils.f90 tree_utils.f90 sort_utils.f90 impurity_measures.f90 classification.f90 predict_wrapper.f90
	gfortran -fPIC -shared utils.f90 tree_utils.f90 sort_utils.f90 impurity_measures.f90 classification.f90 predict_wrapper.f90 -o predict_wrapper.dll


grow_predict_rwrapper.dll: utils.f90 tree_utils.f90 sort_utils.f90 impurity_measures.f90 classification.f90 grow_predict_rwrapper.f90
	gfortran -fPIC -shared utils.f90 tree_utils.f90 sort_utils.f90 impurity_measures.f90 classification.f90 grow_predict_rwrapper.f90 -o grow_predict_rwrapper.dll

testing_program.exe: $(OBJECTS)
	gfortran $(FFLAGS) $(OBJECTS) -o testing_program.exe

%.o : %.f90
	gfortran $(FFLAGS) -c  $< 

clean:
	rm -f *.o *.exe *.mod
