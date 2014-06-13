
Big steps
---------
* Implement decision tree growing in single-threading
	* Write tests
		* for sort_Xcols_Ycorresp()
		* for grow()
* Implement decision tree prediction in single-threading
	* write tests
		* for predict_rec_hlpr()
		* for predict()
* Implement R interface and connect with FORTRAN 90 code
* Implement decision tree fitting in multi-threading
* Implement decision tree estimation in multi-threading

Loose ends
----------
* have comment to indicate when a test is on sorted data only
* review and fix any bad pointer practices
* explicitly indicate public/private access in classification module
* break out classification module into several modules and testing suites
* write better sort to replace insertion sort