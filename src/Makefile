OBJECTS = classification.o testing_program.o
FFLAGS = 

.PHONY: test clean 

test: testing_program.exe
	./testing_program.exe

grow_predict_rwrapper.dll: classification.f90 grow_predict_rwrapper.f90
	gfortran -fPIC -shared classification.f90 grow_predict_rwrapper.f90 -o grow_predict_rwrapper.dll

testing_program.exe: $(OBJECTS)
	gfortran $(FFLAGS) $(OBJECTS) -o testing_program.exe

%.o : %.f90
	gfortran $(FFLAGS) -c  $< 

clean:
	rm -f *.o *.exe *.mod
