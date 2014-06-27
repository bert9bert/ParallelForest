#------------------------------------------------------------------------------
#   Defines a predict method for the class forest to make predictions on
#       fitted forests.
#   Copyright (C) 2014  Bertram Ieong
#------------------------------------------------------------------------------


predict.forest = function(object, xnew){
    # input assertions
    if(class(xnew)!="matrix") stop("Input error.")
    if(typeof(xnew)!="double") stop("Input error.")

    n.new = as.integer(nrow(xnew))
    p = as.integer(ncol(xnew))

    if(p != object@p){
        stop("New data has different number of variables than training data.")
    }

    retpred = .Fortran("predict_forest_wrapper",
        (object@flattened.nodes)$treenum,
        (object@flattened.nodes)$tag,
        (object@flattened.nodes)$tagparent,
        (object@flattened.nodes)$tagleft,
        (object@flattened.nodes)$tagright,
        (object@flattened.nodes)$is_topnode,
        (object@flattened.nodes)$depth,
        (object@flattened.nodes)$majority,
        (object@flattened.nodes)$has_subnodes,
        (object@flattened.nodes)$splitvarnum,
        (object@flattened.nodes)$splitvalue,
        object@numnodes,
        object@numboots,
        n.new,
        p,
        xnew,
        ynew_pred=integer(n.new)
        )

    return(retpred$ynew_pred)
}


setMethod(f = "predict", signature = "forest", definition = predict.forest)
