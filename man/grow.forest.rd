\name{grow.forest}
\alias{grow.forest}
\title{Growing random decision forest classifier}
\description{
	Grow random decision forest classifier
}
\usage{
grow.forest(formula, data, subset, na.action,
    impurity.function = "gini", 
    model = FALSE, x = FALSE, y = FALSE,
    min_node_obs, max_depth, 
    numsamps, numvars, numboots)
}
\arguments{
  \item{formula}{an object of class \code{"\link{formula}"} (or one that
    can be coerced to that class): a symbolic description of the
    model to be fitted.}

  \item{data}{an optional data frame, list or environment (or object
    coercible by \code{\link{as.data.frame}} to a data frame) containing
    the variables in the model.  If not found in \code{data}, the
    variables are taken from \code{environment(formula)},
    typically the environment from which \code{grow.forest} is called.}

  \item{subset}{an optional vector specifying a subset of observations
    to be used in the fitting process.}

  \item{na.action}{a function which indicates what should happen
    when the data contain \code{NA}s.  The default is set by
    the \code{na.action} setting of \code{\link{options}}, and is
    \code{\link{na.fail}} if that is unset.  The \sQuote{factory-fresh}
    default is \code{\link{na.omit}}.  Another possible value is
    \code{NULL}, no action.}

	\item{impurity.function}{the impurity function to be used to fit decision trees, currently only \code{"gini"} is supported.}

  \item{model, x, y}{logicals.  If \code{TRUE} the corresponding
    components of the fit (the model frame, the model matrix, the
    response) are returned.
  }

	\item{min_node_obs}{the minimum number of observations required for a node to be split. If not provided as input, the package will attempt to choose a reasonable value.}
	\item{max_depth}{the deepest that a tree should be fit (root node is at depth 0). If not provided as input, the package will attempt to choose a reasonable value.}
	\item{numsamps}{number of samples to draw with replacement for each tree in the forest (bootstrapped sample). If not provided as input, the package will attempt to choose a reasonable value.}
	\item{numvars}{number of variables to be randomly selected without replacement for each tree in the forest. If not provided as input, the package will attempt to choose a reasonable value.}
	\item{numboots}{number of trees in the forest. If not provided as input, the package will attempt to choose a reasonable value.}
}
\details{
  Bootstrapped samples will be automatically balanced between dependent variable classes. The number of sampled observations per tree will be increased as necessary to achieve a number that can divide the number of dependent variable classes so that bootstrapped samples will be balanced. The number of distinct values that the dependent variable has must be exactly two. Predictor variables should only be continuous, ordinal, or categorical with only two categories (do not include nominal variables or categorical variables with three or more categories).
}
\examples{
  data(easy_2var_data)
  
  fforest = grow.forest(Y~X1+X2, data=easy_2var_data, 
    min_node_obs=5, max_depth=10,
    numsamps=90, numvars=1, numboots=5)
}