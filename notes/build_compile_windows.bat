
:: After building the source package for ParallelForest by running
:: the `build_install_check.sh` bash script in Linux, this
:: batch script can be run in Windows to build and compile
:: a Windows binary package.

set PATH=%PATH%;c:\Rtools\bin;c:\Rtools\MinGW_64\bin;C:\Program Files\R\R-3.5.1\bin

echo Removing old files...
del ParallelForest_1.1.2.zip

echo Build and compile a Windows binary package for current version of R...
R CMD INSTALL --build ParallelForest_1.1.2.tar.gz
