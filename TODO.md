
Big steps
---------

* Implement decision tree prediction in single-threading
	* write base case tests for predict()
* Implement R interface and connect with Fortran 90 code
	* write mechanism to pass trees between R and Fortran
	* write separate grow and predict functions for R
	* write tests for grow and predict implementations in R
	* fit on real dataset with R
* Implement decision tree fitting in multi-threading
* Implement decision tree estimation in multi-threading

Loose ends
----------
* have comment to indicate when a test is on sorted data only
* review and fix any bad pointer practices
* explicitly indicate public/private access in classification module
* break out classification module into several modules and testing suites
* write better sort to replace insertion sort