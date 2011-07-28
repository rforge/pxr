#################################################################
# 
# File:         as.px.array
# Purpose:      Converts an array to a  px object
#
# Created:      20110728
# Authors:      fvf
#
# Modifications: 
#
#################################################################


as.px.array  <- function (obj.array,   skeleton.px=NULL ) {

 
 mkl1<- function (name,value) {
             zz <- list( list(value=value) ) 
             names( zz )  <- name
             return( zz )   }

 if ( is.null(skeleton.px)) {
    opx <- c( mkl1( 'CHARSET','ANSI'),
              mkl1( 'AXIS.VERSION','2000'),
              mkl1( 'TITLE','parameter array name: how?'),
              mkl1( 'CREATION.DATE', format(Sys.time(), "%Y%m%d %H:%M:%S")),
              mkl1( 'LAST.UPDATED', format(Sys.time(), "%Y%m%d %H:%M:%S"))  )
    } else {  opx <- skeleton.px   }
  
 dd <- length( dim(obj.array))
 if (dd > 1) {
     opx <- c(opx,     
             mkl1( 'STUB'   , rev(names(dimnames(obj.array))[2:dd] )),
             mkl1( 'HEADING', names(dimnames(obj.array))[1])   )      }

 opx <- c( opx, list( 'VALUES' = dimnames(obj.array)) )
 opx <- c( opx, list( 'DATA'   = list(value=as.vector(obj.array)) ) )
 class( opx ) <- "px"
  
 return(opx)
  
}

### example

#oo  <- read.px(  system.file( "extdata", "example.px", package = "pxR"))
#aa  <- as.array(oo)
#as.px.array(aa)->px.aa1
#as.px.array(aa,skeleton.px=oo )->px.aa2

#as.array(px.aa1)->aa1
#str(aa) ; str (aa1)

#sum(as.array(aa)-as.array(px.aa1))
#sum(as.array(aa)-as.array(px.aa2))
