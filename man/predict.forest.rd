\name{predict.forest}
\alias{predict.forest}
\alias{predict.tree}
\alias{predict,forest-method}
\alias{predict,tree-method}
\title{Predict method for random decision forest classifier fits}
\description{
  Predict method for random decision forest classifier fits
}
\usage{
\S4method{predict}{forest}(object, newdata, \dots)
}
\arguments{
  \item{object}{Object of class inheriting from \code{"\link{forest-class}"}.}
  \item{newdata}{A data frame in which to look for variables with which to predict.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\examples{
  data(easy_2var_data)

  fforest = grow.forest(Y~X1+X2, data=easy_2var_data, 
    min_node_obs=5, max_depth=10,
    numsamps=90, numvars=1, numboots=5)

  xnew = data.frame(
    X1 = c(0.06, 0.05, 0.05, 0.01, 0.09, 0.05, 0.05,-1000, 1000),
    X2 = c(0.03, 0.02, 0.05, 0.03, 0.04,-1000, 1000, 0.04, 0.03)
    )

  fforest_ynewhat = predict(fforest, xnew)
}