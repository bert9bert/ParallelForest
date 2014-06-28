#------------------------------------------------------------------------------
#   Defines an R function to grow a forest with the compiled underlying
#       Fortran base. Returns an object of class forest.
#   Copyright (C) 2014  Bertram Ieong
#------------------------------------------------------------------------------


grow.forest = function(formula, data, subset, weights, na.action,
    impurity.function = "gini", model = FALSE, x = FALSE, y = FALSE,
    min_node_obs, max_depth, 
    numsamps, numvars, numboots){

    # TODO: implement subset, weights, and na.action

    m = model.frame(formula, data=data)

    ytrain = as.integer(m[,1])
    xtrain = m[,-1]  # TODO: add check that there is no constant term, or string terms

    # get data size
    n = nrow(xtrain)
    p = ncol(xtrain)

    # input assertions

    # TODO: revise these input assertions for new arguments list

    if(length(min_node_obs)!=1) stop("Input error.")
    if(length(max_depth)!=1) stop("Input error.")
    if(length(numsamps)!=1) stop("Input error.")
    if(length(numvars)!=1) stop("Input error.")
    if(length(numboots)!=1) stop("Input error.")



    # determine the maximum possible number of nodes with the given max depth for the 
    # fitted tree, which determines the length of the padded array that the Fortran
    # subroutine should return
    TOP_NODE_NUM = 0
    retlen = 2^(max_depth + 1 - TOP_NODE_NUM) - 1

    # send to Fortran wrapper to grow forest
    ret = .Fortran("grow_forest_wrapper",
        n=as.integer(n), p=as.integer(p),
        xtrain=as.matrix(xtrain), ytrain=as.integer(ytrain),
        min_node_obs=as.integer(min_node_obs), max_depth=as.integer(max_depth), 
        retlen=as.integer(retlen),
        numsamps=as.integer(numsamps),
        numvars=as.integer(numvars),
        numboots=as.integer(numboots),
        treenum_padded=integer(retlen*numboots),
        tag_padded=integer(retlen*numboots),
        tagparent_padded=integer(retlen*numboots),
        tagleft_padded=integer(retlen*numboots),
        tagright_padded=integer(retlen*numboots),
        is_topnode_padded=integer(retlen*numboots),
        depth_padded=integer(retlen*numboots),
        majority_padded=integer(retlen*numboots),
        has_subnodes_padded=integer(retlen*numboots),
        splitvarnum_padded=integer(retlen*numboots),
        splitvalue_padded=double(retlen*numboots),
        numnodes=integer(numboots)
        )

    # unpad returned arrays and put everything into a forest object
    flattened.nodes = data.frame(
        treenum=ret$treenum_padded[1:sum(ret$numnodes)],
        tag=ret$tag_padded[1:sum(ret$numnodes)],
        tagparent=ret$tagparent_padded[1:sum(ret$numnodes)],
        tagleft=ret$tagleft_padded[1:sum(ret$numnodes)],
        tagright=ret$tagright_padded[1:sum(ret$numnodes)],
        is_topnode=ret$is_topnode_padded[1:sum(ret$numnodes)],
        depth=ret$depth_padded[1:sum(ret$numnodes)],
        majority=ret$majority_padded[1:sum(ret$numnodes)],
        has_subnodes=ret$has_subnodes_padded[1:sum(ret$numnodes)],
        splitvarnum=ret$splitvarnum_padded[1:sum(ret$numnodes)],
        splitvalue=ret$splitvalue_padded[1:sum(ret$numnodes)]
        )

    fitted.forest = new("forest",
        n=ret$n, p=ret$p,
        min_node_obs=ret$min_node_obs, max_depth=ret$max_depth,
        numsamps=ret$numsamps,
        numvars=ret$numvars,
        numboots=ret$numboots,
        numnodes=ret$numnodes,
        flattened.nodes=flattened.nodes,
        fmla=formula
        )

    return(fitted.forest)
}

