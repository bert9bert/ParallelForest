
Big steps
---------

* Implement R interface to connect with Fortran 90 code
	* fit on real dataset with R
	* make sure that xtrain and xtest integer matrices can be converted to double matrices
	* account for missing values
	* put in functionality with data frames in addition to matrices

Loose ends
----------
* have comment to indicate when a test is on sorted data only, and write unsorted tests
* review and fix any bad pointer practices
* explicitly indicate public/private access in modules
* write better sort to replace insertion sort
* figure how to pass optional arguments such as num threads to wrappers
* look into why splitvalue is sometimes NaN


