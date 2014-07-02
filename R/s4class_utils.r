#------------------------------------------------------------------------------
#   Utilities for S4 classes.
#   Copyright (C) 2014  Bertram Ieong
#------------------------------------------------------------------------------


setOldClass("data.frame")
setClassUnion("data.frameORvector", c("data.frame", "vector"))
