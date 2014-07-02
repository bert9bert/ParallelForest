\name{grow.forest}
\alias{grow.forest}
\title{Growing random decision forest classifier}
\description{
	Growing random decision forest classifier
}
\usage{
grow.forest(formula, data, subset, na.action,
    impurity.function = "gini", 
    model = FALSE, x = FALSE, y = FALSE,
    min_node_obs, max_depth, 
    numsamps, numvars, numboots)
}
\arguments{
	\item{formula}{an object of class ‘“formula”’ (or one that can be coerced to
          that class): a symbolic description of the model to be
          fitted.}
	\item{data}{an optional data frame, list or environment (or object
          coercible by ‘as.data.frame’ to a data frame) containing the
          variables in the model.  If not found in ‘data’, the
          variables are taken from ‘environment(formula)’, typically
          the environment from which ‘lm’ is called.}
	\item{subset}{an optional vector specifying a subset of observations to be
          used in the fitting process.}
	\item{na.action}{a function which indicates what should happen when the data
          contain ‘NA’s.  The default is set by the ‘na.action’ setting
          of ‘options’, and is ‘na.fail’ if that is unset.  The
          ‘factory-fresh’ default is ‘na.omit’.  Another possible value
          is ‘NULL’, no action.  Value ‘na.exclude’ can be useful.}
	\item{impurity.function}{the impurity function to be used to fit decision trees, currently only ‘impurity.function = “gini”’ is supported.}
	\item{model, x, y}{If ‘TRUE' the corresponding components of
          the fit (the model frame, the design matrix, the response) are returned.}
	\item{min_node_obs}{the minimum number of observations required for a node to be split.}
	\item{max_depth}{the deepest that a tree should be fit (root node is at depth 0).}
	\item{numsamps}{number of samples to draw with replacement for each tree in the forest (bootstrapped sample).}
	\item{numvars}{number of variables to be randomly selected without replacement for each tree in the forest.}
	\item{numboots}{number of trees in the forest.}
}
\details{
  Bootstrapped samples will be automatically balanced between dependent variable classes. Dependent variable must be automatically coercible to 0 and 1. Predictor variables should only be continuous, ordinal, or categorical with only two categories (do not include nominal/categorical variables with three or more categories).
}
