Package Description
===================

This project is an R package for fitting decision trees using parallel computing, with Fortran and OpenMP under the hood.

To-Do List
==========

* Implement single-node splitting function
	* Write test for case with >1 variables, with one test for a best split on not the first variable and one test for a best split on not the last variable
	* Write test for base cases
* Implement decision tree growing in single-threading
	* Implement recursive components and node pointing
	* Implement grow function
	* Write tests
* Implement decision tree prediction in single-threading
* Implement R interface and connect with FORTRAN 90 code
* Implement decision tree fitting in multi-threading
* Implement decision tree estimation in multi-threading


