#------------------------------------------------------------------------------
#   Defines a predict method for the class forest to make predictions on
#       fitted forests.
#   Copyright (C) 2014
#   No warranty provided.
#------------------------------------------------------------------------------

# method to be used for predict function in forest S4 class
predict.forest = function(object, newdata, type="binary_0_1", decision_threshold=0.5, ...){

    ## set debugging variables
    verbose = FALSE

    ## validate input options ###
    if(!(type %in% c("binary_0_1","response"))){
      stop("type argument needs to be either binary_0_1 or response")
    }

    stopifnot(decision_threshold >= 0)
    stopifnot(decision_threshold <= 1)

    ### Create design matrix ###
    fmla.str = deparse(object@fmla)
    Yvar.str = strsplit(fmla.str," ~ ")[[1]][1]

    newdata.tmp = newdata
    newdata.tmp[,Yvar.str] = rep(-1,nrow(newdata.tmp))

    xtest = model.frame(object@fmla, data=newdata.tmp)[,-1]
    xtest.tof = as.matrix(xtest)
    storage.mode(xtest.tof) = "double"

    # get new data size #
    n.new = as.integer(nrow(xtest))
    p = as.integer(ncol(xtest))

    if(p != object@p){
        stop("New data has different number of variables than training data.")
    }

    ### Send to compiled Fortran wrapper to get forest prediction ###
    retpred = .Fortran("predict_forest_wrapper",
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
        xtest.tof,
        ynew_pred_01=double(n.new)
        )

    ### debugging output
    if (verbose) {
        cat("\nPredicted outputs that Fortran gives to R: \n")
        cat(paste(retpred$ynew_pred_01, collapse=", "))
        cat("\n")
    }

    ### get vector with forest prediction in the probability space ###
    #ynew_pred = do.call(prep.depvar.out, append(list(retpred$ynew_pred_01), object@depvar.restore.info))
    ynew_pred = retpred$ynew_pred_01



    ### apply decision rule if needed ###
    if (type=="binary_0_1") {
        ynew_pred = as.integer(ynew_pred > decision_threshold)
    }

    ### return ###
    stopifnot(ynew_pred >= 0)
    stopifnot(ynew_pred <= 1)

    return(ynew_pred)
}

# set the predict function with the method defined above for the forest S4 class
setMethod(f = "predict", signature = "forest", definition = predict.forest)
