#################################################################
# 
# File:         as.array.px.R
# Purpose:      Converts a px object to an array
#
# Created:      20110618
# Authors:      fvf
#
# Modifications: opl 
#
#################################################################

as.array.px <- function(x,... ){
  
    names.vals      <- c( x$HEADING$value, rev( x$STUB$value ) )
    
    result <- array( x$DATA[[1]], unlist(lapply(x$VALUES[names.vals],length)), dimnames=x$VALUES[names.vals] )
    names( dimnames(result) ) <- names.vals
    
    return(result)
  
}


