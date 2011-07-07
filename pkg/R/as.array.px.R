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
  names.vals <- c( x$HEADING$value, rev( x$STUB$value ) )
  ## Reordena los valores para que cuadren con los datos
  ord <- match(names.vals, names(x$VALUES)) 
  ## Crea array
  result <- array( x$DATA$value, 
                  unlist(lapply(x$VALUES[ord],length)),
                  dimnames=x$VALUES[ord] )
##  names( result ) <- names.vals

  result
}
