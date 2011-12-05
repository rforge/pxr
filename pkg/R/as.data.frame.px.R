#################################################################
# 
# File:         as.data.frame.px.R 
# Purpose:      extracts a df from a px object
#
# Created:      20110801
# Authors:      cjgb, opl, fvf
#
# Modifications: 
#
#################################################################

as.data.frame.px <- function( x, row.names, optional, use.codes = FALSE, ... ){
  names.vals      <- c( rev(x$HEADING$value), rev( x$STUB$value ) )

  values <- x$VALUES

  if( is.logical( use.codes ) && use.codes )
    use.codes <- names( x$CODES )

  if( ! is.logical( use.codes ) ){
    for( var.name in intersect( use.codes, names( x$CODES ) ) ){
      if( var.name %in% names.vals ) {
        var.name.value <- var.name
      } else {
        var.name.value <- agrep(var.name, names.vals, value="TRUE")
      }
      values[[var.name.value]] <- x$CODES[[var.name]]
      ##            values[[var.name]] <- x$CODES[[ var.name ]] 
    }
  }

  result          <- data.frame( do.call(expand.grid, values[ names.vals ] ), x$DATA$value )
  names( result ) <- c( names.vals, "dat" )

  result
}

