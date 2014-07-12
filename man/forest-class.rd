\name{forest-class}
\Rdversion{1.1}
\docType{class}
\alias{forest-class}


\title{Class \code{"forest"}}
\description{
A forest of decision tree classifiers to be used for ensemble prediction.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("forest", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{n}:}{Number of observations in dataset used to fit this forest.}
    \item{\code{p}:}{Number of independent variables in dataset used to fit this forest.}
    \item{\code{min_node_obs}:}{Leaf of any tree in this forest will not be split unless it has more observations than this value.}
    \item{\code{max_depth}:}{Maximum depth of any tree in this forest}
    \item{\code{numsamps}:}{Number of observations randomly drawn with replacement used to fit a tree in this forest.}
    \item{\code{numvars}:}{Number of independent variables randomly drawn without replacement used to fit a tree in this forest.}
    \item{\code{numboots}:}{Number of trees in this forest.}
    \item{\code{numnodes}:}{Vector with the number of nodes that each tree has in this forest.}
    \item{\code{flattened.nodes}:}{Data frame containing information on the nodes of the trees in this forest.}
    \item{\code{model}:}{Model frame used to fit this forest.}
    \item{\code{x}:}{Design (independent variables) matrix used to fit this forest.}
    \item{\code{y}:}{Dependent variable vector used to fit this forest.}
    \item{\code{fmla}:}{Formula used to construct the model frame from the data.}
    \item{\code{depvar.restore.info}:}{This is a slot that the package needs internally.}
  }
}

\keyword{classes}
