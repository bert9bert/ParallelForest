#------------------------------------------------------------------------------
#   Defines an R function to grow a tree with the compiled underlying
#       Fortran base. Returns an object of class tree.
#   Copyright (C) 2014  Bertram Ieong
#------------------------------------------------------------------------------


grow.tree = function(formula, data, subset, weights, na.action,
    impurity.function = "gini", model = FALSE, x = FALSE, y = FALSE,
    min_node_obs, max_depth){

    # TODO: implement subset, weights, and na.action

    m = model.frame(formula, data=data)

    ytrain = as.integer(m[,1])
    xtrain = m[,-1]  # TODO: add check that there is no constant term, or string terms

    ytrain.tof = as.integer(ytrain)
    xtrain.tof = as.matrix(xtrain)
    storage.mode(xtrain.tof) = "double"

    # get data size
    n = nrow(xtrain)
    p = ncol(xtrain)

    # input assertions

    # TODO: revise these input assertions for new arguments list

    if(length(min_node_obs)!=1) stop("Input error.")
    if(length(max_depth)!=1) stop("Input error.")




    # determine the maximum possible number of nodes with the given max depth for the 
    # fitted tree, which determines the length of the padded array that the Fortran
    # subroutine should return
    TOP_NODE_NUM = 0
    retlen = 2^(max_depth + 1 - TOP_NODE_NUM) - 1

    # send to Fortran wrapper to grow forest
    ret = .Fortran("grow_wrapper",
        n=as.integer(n), p=as.integer(p),
        xtrain=xtrain.tof, ytrain=ytrain.tof,
        min_node_obs=as.integer(min_node_obs), max_depth=as.integer(max_depth), 
        retlen=as.integer(retlen),
        tag_padded=integer(retlen),
        tagparent_padded=integer(retlen),
        tagleft_padded=integer(retlen),
        tagright_padded=integer(retlen),
        is_topnode_padded=integer(retlen),
        depth_padded=integer(retlen),
        majority_padded=integer(retlen),
        has_subnodes_padded=integer(retlen),
        splitvarnum_padded=integer(retlen),
        splitvalue_padded=double(retlen),
        numnodes=integer(1)
        )

    # unpad returned arrays and put everything into a tree object
    flattened.nodes = data.frame(
        tag=ret$tag_padded[1:ret$numnodes],
        tagparent=ret$tagparent_padded[1:ret$numnodes],
        tagleft=ret$tagleft_padded[1:ret$numnodes],
        tagright=ret$tagright_padded[1:ret$numnodes],
        is_topnode=ret$is_topnode_padded[1:ret$numnodes],
        depth=ret$depth_padded[1:ret$numnodes],
        majority=ret$majority_padded[1:ret$numnodes],
        has_subnodes=ret$has_subnodes_padded[1:ret$numnodes],
        splitvarnum=ret$splitvarnum_padded[1:ret$numnodes],
        splitvalue=ret$splitvalue_padded[1:ret$numnodes]
        )

    fitted.forest = new("forest",
        n=ret$n, p=ret$p,
        min_node_obs=ret$min_node_obs, max_depth=ret$max_depth,
        numnodes=ret$numnodes,
        flattened.nodes=flattened.nodes,
        fmla=formula
        )

    return(fitted.forest)
}

