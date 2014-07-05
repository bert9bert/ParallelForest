
For this version
================
* finish writing intro vignette
* proofread manuals




For next version
================
* Implement more memory friendly way to pass tree/forest from compiled Fortran to R
* In classification.f90:365-368, investigate whether memory will build up in the stack because of the many allocations of Xleft, etc.
* Review and fix any bad pointer practices
* Explicitly indicate public/private access in modules
* Implement sanity check in tree_utils.f90 to make sure erroneous nodes aren't created
* Make balanced bootstrap more memory friendly
* Make countnodes() API more elegant
* Have predict.forest create its model frame (in particular, its design matrix) in a more memory friendly way
* Implement passing of optional arguments such as the number of threads to use to wrappers
* Implement changing to non-default thread number
* In wrappers, look into using R logicals for returned Fortran logicals
* Return bootstrapped numsamps actually used
