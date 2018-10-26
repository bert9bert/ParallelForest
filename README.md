# ParallelForest R Package

ParallelForest is an R package implementing random forest classification using parallel computing, built with Fortran and OpenMP in the backend.

## Getting Started
### Installation
Install ParallelForest by entering the following into an R console.
```R
install.packages("devtools")
devtools::install_github("bert9bert/ParallelForest")
library(ParallelForest)
```
This will use the latest source files maintained on GitHub to compile and install the package on the user's side. Alternative installation methods using pre-compiled binary packages are kept in the `prebuilt_packages/` directory. The ParallelForest package is also available on CRAN, but the version on GitHub (here) may be more current.

### Basic Usage
The `grow.forest` function is used to grow (fit) a random forest, which returns a fitted forest object. Afterwards, the `predict` function can be used to make predictions on the fitted forest with new data. The following commands can be used to view documentation for these functions.
```R
?grow.forest             # get the help page for growing a forest
?predict.forest          # get the help page for predicting on a fitted forest
?accessors.forest        # get help page for accessing elements of a fitted forest,
                         #     e.g. fittedforest["min_node_obs"]
```

## A Simple Example with U.S. Census Data
A dataset of income and other person-level characteristics based off the U.S. Census Bureau's Current Population Surveys in 1994 and 1995 can be downloaded from the UCI Machine Learning Repository [here](http://archive.ics.uci.edu/ml/datasets/Census-Income+%28KDD%29). The dependent variable is binary and indicates whether the survey-taker's income is under or over $50,000. Seven predictor variables are used: age, wage per hour, capital gains, capital losses, dividends from stocks, number of persons worked for employer, and weeks worked in year. This dataset is split into a training dataset and a testing (hold-out) dataset.

First, load the package into R, then load the training and testing datasets.

```{r}
library(ParallelForest)

data(low_high_earners)       # cleaned and prepared training dataset (199,522 observations)
data(low_high_earners_test)  # cleaned and prepared testing dataset (99,761 observations)
```

### Fitting a Forest

Then fit a forest to the training data. A variety of hyper-parameters can be specified to control how the forest will be fit, which are documented in the help file (see `?grow.forest` in R). If a hyper-parameter is not specified then the package may try to choose reasonable values for it.
```{r}
fforest = grow.forest(
  Y~.,
  data=low_high_earners,
  min_node_obs = 1000,      # min obs to split node
  max_depth = 10,           # max tree depth
  numsamps = 100000,        # num obs to draw for each tree
  numvars = 5,              # num vars to draw for each tree
  numboots = 100            # number of trees in the forest
  )
```

### Prediction on the Training Data and on New Data

Then use the fitted forest to get predictions on the training data.
```{r}
fforest_train_pred = predict(fforest, low_high_earners)
```

Now use the fitted forest to get predictions on the testing data.
```{r}
fforest_test_pred = predict(fforest, low_high_earners_test)
```

For both the predictions on the training data and testing data, compute the percentage of observations predicted correctly, the accuracy rate.
```R
pctcorrect_train = sum(low_high_earners$Y==fforest_train_pred)/nrow(low_high_earners)
pctcorrect_test = sum(low_high_earners_test$Y==fforest_test_pred)/nrow(low_high_earners_test)

print(paste0("Model accuracy on the training data is ", round(pctcorrect_train, digits=3)*100, "%."))
print(paste0("Model accuracy on the test (hold-out) data is ", round(pctcorrect_test, digits=3)*100, "%."))
```
> Model accuracy on the training data is 72.5%.  
> Model accuracy on the test (hold-out) data is 71.8%.

## Exploring the Source Code

Source code is located in two places:
* Fortran source code is located in `src/`
* R source code is located in `R/`

## Notes

The use of this packages comes with no warranty and no support.
