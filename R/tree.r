#------------------------------------------------------------------------------
#   Creates an S4 class to represent a tree.
#   Copyright (C) 2014  Bertram Ieong
#------------------------------------------------------------------------------


setClass("tree", representation(    n = "integer",
                                    p = "integer",
                                    min_node_obs = "integer",
                                    max_depth = "integer",
                                    numnodes = "integer",
                                    flattened.nodes = "data.frame"))
