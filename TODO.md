
Big steps
---------

* Implement decision tree prediction in single-threading
	* write base case tests for predict()
* Implement R interface and connect with Fortran 90 code
	* write R functions to use Fortran grow and predict wrappers, write tests
	* fit on real dataset with R
* Implement random forest in single-threading
* Connect random forest with R
* Multi-thread random forest

Loose ends
----------
* have comment to indicate when a test is on sorted data only
* review and fix any bad pointer practices
* explicitly indicate public/private access in classification module
* write better sort to replace insertion sort