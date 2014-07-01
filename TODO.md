
High Priority
-------------
* Implement R interface to connect with Fortran 90 code
	* test on real dataset with R
	* account for missing values
	* finish implementing forest grow and predict APIs
	* sufficiently implement private tree grow and predict APIs to perform tests
	* write manual pages

Medium Priority
----------
* replace insertion sort with more CPU friendly sort
* in classification.f90:365-368, investigate whether memory will build up in the stack because of the many allocations of Xleft, etc.
* implement exception handling system, delete prints and stops
* Make sure that package passes checks for all OSs, including appropriate make files
	* Linux
	* Mac
	* Windows

Low Priority
------------
* Implement tree grow and predict APIs
* review and fix any bad pointer practices
* explicitly indicate public/private access in modules
* implement passing of optional arguments such as num threads to wrappers
* implement changing to non-default thread number
* store testing data in R format
* in wrappers, look into using R logicals for returned Fortran logicals
* implement sanity check in tree_utils.f90 to make sure erroneous nodes aren't created
* make balanced bootstrap more memory friendly
* make countnodes() API more elegant
* implement more memory friendly way to pass tree/forest from compiled Fortran to R