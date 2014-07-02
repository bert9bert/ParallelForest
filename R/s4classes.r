#------------------------------------------------------------------------------
#   This file defines S4 classes that are used in this package.
#   Copyright (C) 2014  Bertram Ieong
#------------------------------------------------------------------------------

setOldClass("data.frame")
setClassUnion("data.frameORvector", c("data.frame", "vector"))


setClass("forest", representation(  n = "integer",
                                    p = "integer",
                                    min_node_obs = "integer",
                                    max_depth = "integer",
                                    numsamps = "integer",
                                    numvars = "integer",
                                    numboots = "integer",
                                    numnodes = "integer",
                                    flattened.nodes = "data.frame",
                                    model = "data.frame",
                                    x = "data.frameORvector",
                                    y = "vector",
                                    fmla = "formula"
                                    ))


setClass("tree", representation(    n = "integer",
                                    p = "integer",
                                    min_node_obs = "integer",
                                    max_depth = "integer",
                                    numnodes = "integer",
                                    flattened.nodes = "data.frame",
                                    fmla = "formula"
                                    ))
