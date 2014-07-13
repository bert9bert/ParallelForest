

To Do List
==========

Feature Expansions
------------------
* Let input `data` field be blank
* Implement passing of optional arguments such as the number of threads to use to wrappers
* Implement changing to non-default thread number
* Return bootstrapped numsamps actually used
* Put in more impurity functions

Performance Improvements
------------------------
* **Fix warnings with `-Wall` compilation**
* Have random number generator seed passed from R to Fortran
* Implement more memory friendly way to pass tree/forest from compiled Fortran to R
* In `classification.f90:365-368`, investigate whether memory will build up in the stack because of the many allocations of `Xleft`, etc.
* Review and fix any bad pointer practices
* Explicitly indicate public/private access in modules
* Implement sanity check in `tree_utils.f90` to make sure erroneous nodes aren't created
* Make balanced bootstrap more memory friendly
* Have `predict.forest` create its model frame (in particular, its design matrix) in a more memory friendly way
* In `grow_forest`, unpack array of node pointers so that the original is destroyed and so that parent is nullified

