#################################################################
# 
# File:         as.data.frame.px.R 
# Purpose:      extracts a df from a px object
#
# Created:      20110618
# Authors:      cjgb, opl, fvf
#
# Modifications: 
#
#################################################################

as.data.frame.px <- function( x, row.names, optional,... ){
    names.vals      <- c( x$HEADING$value, rev( x$STUB$value ) )

    result          <- data.frame( do.call(expand.grid, x$VALUES[ names.vals ] ), x$DATA$value )
    names( result ) <- c( names.vals, "dat" )

    result
}

# test against some INE's PC-Axis files

# province.numbers <- sapply( 1:52, function( x ) sprintf( "%05d", x ) )
# province.numbers <- paste( "http://www.ine.es/pcaxisdl//t20/e245/p05/a2010/l0/", province.numbers, "001.px", sep = "" )

# res <- sapply( province.numbers, function( x ) get.data.px( read.px( url( x ) ) ), simplify = F )

# debug( read.px )

# res <- read.px( url( province.numbers[1] ) )

# res.dat <- get.data.px( res )              # se puede comparar 
                                           # contra http://www.ine.es/jaxi/tabla.do?path=/t20/e245/p05/a2010/l0/&file=00001001.px&type=pcaxis&L=0

