#!/bin/bash

# Script to build, install, and check the ParallelForest package.
# Run `./build_install_check.sh` to use the current version of R installed
# Run `./build_install_check.sh devel` to use the development version of R installed

echo Removing old files...
rm ParallelForest_1.1.2.tar.gz
rm -r ParallelForest.Rcheck/

if [ "$1" != "devel" ]; then
	echo Build, install, and check package for current version of R...
	R CMD build ParallelForest
	R CMD INSTALL --build ParallelForest_1.1.2.tar.gz
	R CMD check --as-cran ParallelForest_1.1.2.tar.gz
else
	echo Build, install, and check package for development version of R...
	R-devel CMD build ParallelForest
	R-devel CMD INSTALL ParallelForest_1.1.2.tar.gz
	R-devel CMD check --as-cran ParallelForest_1.1.2.tar.gz
fi
