## Pre-built Packages

This folder provides pre-built packages as an alternative to the regular installation method via `install_github("bert9bert/ParallelForest")`.

To do so, download the appropriate pre-built package file from the `prebuilt_packages/` folder and run the following command in R after replacing `"path/to/file"` with the path to the downloaded package file.
```R
install.packages("path/to/file", repos=NULL)
```

See the table below to find the appropriate pre-built package file.

| Platform          | Pre-built Package File                             | Notes                                                                                     |
|-------------------|----------------------------------------------------|-------------------------------------------------------------------------------------------|
| Windows (64-bit)  | ParallelForest.zip                                 | pre-compiled binary package, requires no compilation on the user's side                   |
| Linux (64-bit)    | ParallelForest_1.1.1_R_x86_64-pc-linux-gnu.tar.gz  | pre-compiled binary package, requires no compilation on the user's side                   |
| Source            | ParallelForest_1.1.1.tar.gz                        | source package, so not platform-specific, however requires compilation on the user's end  |
