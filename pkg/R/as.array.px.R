#################################################################
# 
# File:         as.array.px.R
# Purpose:      Converts a px object to an array
#
# Created:      20110618
# Authors:      FVF
#
# Modifications: OPL 
#
#################################################################

as.array.px <- function(x,... ){
  
    names.vals      <- c( x$HEADING$value, rev( x$STUB$value ) ) ; str(names.vals)
    
    #  create array
    result <- array( x$DATA[[1]], unlist(lapply(x$VALUES[names.vals],length)), dimnames=x$VALUES[names.vals] )
    names( dimnames(result) ) <- names.vals
    
    return(result)
  
}


# test against some INE's PC-Axis files
# province.numbers <- sapply( 1:5, function( x ) sprintf( "%05d", x ) )
# province.numbers <- paste( "http://www.ine.es/pcaxisdl//t20/e245/p05/a2010/l0/", province.numbers, "001.px", sep = "" )

#  debug( read.px )
# res <- read.px( url( province.numbers[1] ) )
# res.dat <- as.array.px( res )  # compare: 
                                 # http://www.ine.es/jaxi/tabla.do?path=/t20/e245/p05/a2010/l0/&file=00001001.px&type=pcaxis&L=0
# str(res.dat)

# res <- sapply( province.numbers, function( x ) as.array.px( read.px( url( x ) ) ), simplify = F )
