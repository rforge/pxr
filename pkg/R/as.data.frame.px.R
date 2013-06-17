#################################################################
# 
# File:         as.data.frame.px.R 
# Purpose:      extracts a df from a px object
#
# Created:      20110801
# Authors:      cjgb, opl, fvf
#
# Modifications: 
#    20120323, cjgb: added error check: variables and codes should have the same length
#    20120402, cjgb: warnings can be either errors or warnings depending on paranoia level
#
#################################################################

as.data.frame.px <- function( x, ..., use.codes = FALSE, warnings.as.errors = TRUE, direction = 'long'){

  # stores the user settings and resets them on exit
  initial.warning.option <- options("warn")$warn
  on.exit( options(warn=initial.warning.option) )   

  if ( warnings.as.errors )
    options(warn=2)

  if ('KEYS' %in% names(x)) {
    
    result <- pxK2df (x, use.codes) 
    
  } else {
    
    names.vals      <- c( rev(x$HEADING$value), rev( x$STUB$value ) )
    values <- x$VALUES

    if (is.logical(use.codes) && use.codes)
      use.codes <- names(x$CODES)

    if (! is.logical(use.codes)){
      for( var.name in intersect( use.codes, names(x$CODES) ) ){
        if( var.name %in% names.vals ) {
          var.name.value <- var.name
        } else {
          var.name.value <- agrep(var.name, names.vals, value="TRUE")
          if (length(var.name.value) > 0 ){
            var.name.value <- var.name.value[1]      # there could be >1 matches!
            warning( "Using codes tagged ", var.name , " for variable ", var.name.value, " after fuzzy matching.")
          } else {
            stop( "No valid variable name could be found for code tagged ", var.name )
          }
        }

        if( length( values[[var.name.value]] ) != length( x$CODES[[var.name]] ) )
            stop( "The number of levels and codes for variable '", var.name.value, "' should be the same, but are ", 
            length( values[[var.name.value]] ), " and ", length( x$CODES[[var.name]] ), " respectively" )

        values[[var.name.value]] <- x$CODES[[var.name]]
        ##            values[[var.name]] <- x$CODES[[ var.name ]] 
      }
    }
    
    # sanity check: avoids the problem of "reclycling" of values if
    # the ratio of lenghts of variables and values is an exact integer
    
    dat <- data.frame(do.call(expand.grid, values[names.vals]))
    if (nrow(dat) != length(x$DATA$value))
       stop( "The input file is malformed: data and varnames length differ" )
  
    dat        <- data.frame(dat, x$DATA$value)
    names(dat) <- c(names.vals, "dat")
    
  
    if (direction == 'wide') {
      stub    <- x$STUB$value
      heading <- x$HEADING$value
      vars    <- x$VALUES[heading]
      result  <- reshape(dat, timevar=heading,
                        varying=vars,
                        v.names='dat',
                        idvar=stub,
                        direction='wide')
    } else {
      result <- dat
    }
  }
  
  result
}

