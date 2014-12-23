#################################################################
#
# File:         as.px.data.frame
# Purpose:      Converts an data.frame to a  px object
#
# Created:      20141209
# Authors:      fvf
#
# Modifications:
#               [141209] fvf
#                 
#
#################################################################


as.px.data.frame  <- function ( x, skeleton.px = NULL, list.keys = NULL,
                                value.colum = NULL,  ...  )

{

  
 if ( !  is.data.frame (x))
    stop('Error: object is not a "data.frame"')

 if ( ! is.null(value.colum) ) {
   if (  value.colum  %in%   names (x)  ) {
       names(x)[names(x) %in%   value.colum[1]] <- 'value'
   } else   stop(paste('Error: There is not a',  value.column,  'in "data.frame"'))
 }
  
 if ( ! 'value' %in%   names (x) )
    stop('Error: There is not a colum name "value" in "data.frame" or value.column unspecified')

  
  ## auxiliary functions
  
  mkl1 <- function (key, value)         {
    zz <- list( list(value=value) )
    names( zz ) <- key
    zz
  }
  
  ## default skeleton
  
  if ( is.null( opx <- skeleton.px ) )
    opx <- c( mkl1( 'CHARSET','ANSI'),
              mkl1( 'MATRIX','file000'),
              mkl1( 'AXIS-VERSION','2000'),
              mkl1( 'SUBJECT-CODE','xx'),
              mkl1( 'SUBJECT-AREA','unknown'),
              mkl1( 'CONTENTS'    ,'unknown'),
              mkl1( 'UNITS'       ,'unknown'),
              mkl1( 'TITLE'       ,'Title unknown'),
              mkl1( 'DECIMALS', 0),
              mkl1( 'CREATION-DATE', format(Sys.time(), "%Y%m%d %H:%M:%S")),
              mkl1( 'LAST-UPDATED', format(Sys.time(), "%Y%m%d %H:%M:%S"))  )

  
  ## Add key-value pair if list.keys<>NULL
  if (! is.null(list.keys)) 
    for (key in names(list.keys))
      opx[key] <- list( list(value = list.keys[[key]]) )
      
  
  # delete STUB, HEADING, VALUES and DATA if present
  opx['STUB'] <- opx['HEADING'] <- opx['VALUES'] <- opx['DATA'] <- NULL

  names.val <- names(x)[!(names(x) %in% 'value')]
  
  dd <- length(names.val)
  if (dd > 1) {
    # First dim to 'header', the rest in reverse order
    opx[["STUB"]]    <- list( value = names.val[-dd])
    opx[["HEADING"]] <- list( value = names.val[dd])
  }   else  opx[["STUB"]] <- names.val[dd]  # Only one dim


  opx$VALUES <- NULL

  for (i in names.val) {
    opx$VALUES <- c(opx$VALUES,list(levels(x[[i]])))    
  } 
 
  names(opx$VALUES) <- names.val

  # Si el data.frame incluye un atributo codes
  if ( ! is.null (attr(x,'CODES') )) {
    
    opx$CODES <- NULL
    codes <- attr(x,'CODES')
    for (i in names(codes)) {
      opx$CODES <- c(opx$CODES,list(codes[[i]] ))    
    }
    names(opx$CODES)  <- names(codes)
    
    attr(x,'CODES') <- NULL
  
  }

  

  opx$DATA$value  <- x
  
  
  #  Delete skeleton.px$CODES: unused or inconsistent
  
  if ( ! is.null(opx$CODES) ) {
    
    new.codes <- opx$CODES
    opx$CODES <- list()
    
    new.codes <- new.codes [names(new.codes) %in% names(opx$VALUES)]
    
    new.codes <- new.codes[
      sapply(new.codes, length) ==
        sapply(opx$VALUES[names(new.codes)] ,length)
      ]
    
    opx$CODES <- new.codes
    
  }
  
  
 
  class( opx ) <- "px"
  
  opx

}



