
High Priority
-------------
* Implement R interface to connect with Fortran 90 code
	* account for missing values
	* finish implementing forest grow and predict APIs

Medium Priority
----------
* Make sure that package passes checks for all OSs, including appropriate make files
	* Linux
	* Mac
	* Windows
* implement exception handling system, delete prints and stops
* replace insertion sort with more CPU friendly sort
* in classification.f90:365-368, investigate whether memory will build up in the stack because of the many allocations of Xleft, etc.


Low Priority
------------
* store testing data in (compressed) R format
* implement more memory friendly way to pass tree/forest from compiled Fortran to R
* Implement tree grow and predict APIs
* review and fix any bad pointer practices
* explicitly indicate public/private access in modules
* implement passing of optional arguments such as num threads to wrappers
* implement changing to non-default thread number
* in wrappers, look into using R logicals for returned Fortran logicals
* implement sanity check in tree_utils.f90 to make sure erroneous nodes aren't created
* make balanced bootstrap more memory friendly
* make countnodes() API more elegant
