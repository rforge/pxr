#################################################################
# 
# File:         as.px.array
# Purpose:      Converts an array to a  px object
#
# Created:      20110801
# Authors:      fvf, cjgb, opl
#
# Modifications: 
#
#################################################################


as.px.array  <- function ( x, skeleton.px = NULL, list.keys = NULL, ...  )
# skeleton.px = Utiliza un objeto px preexistente para heredar sus claves,
#               menos: STUB,HEADING,VALUES (y habra que implementar CODES)
# listkey     = Admite una lista con pares (key = value), que se puede
#               utilizar para rellenar claves obligatorias y no obligatorias.
#               Recomendable:
#                   list(MATRIX='filename', CONTENTS='unknown', CONTEXTS'='unknown',
#                        UNITS'='unknown', TITLE'='Title unknown', DECIMAL','0')
#
# Pruebas:
# x<-aa
# skeleton.px <- oo
# skeleton.px <- NULL
# list.keys= list(MATRIX='xxxx', CONTENTS='cosas', MIA='prueba',
#                 UNITS='personas', TITLE='titulos', DECIMAL='1') -> list.keys  
{

  if ( ! inherits(x , "array" ) )
  stop('Error: object is not a "array"')
 
  mkl1<- function (name,value)         {           
              zz <- list( list(value=value) ) 
              names( zz )  <- name
              return( zz )
           }

 if ( is.null(skeleton.px)) {
    opx <- c( mkl1( 'CHARSET','ANSI'),
              mkl1( 'MATRIX','file000'),             
              mkl1( 'AXIS-VERSION','2000'),
              mkl1( 'SUBJECT-CODE','xx'),
              mkl1( 'SUBJECT-AREA','unknown'),
              mkl1( 'CONTENTS'    ,'unknown'),
              mkl1( 'UNITS'       ,'unknown'),                          
              mkl1( 'TITLE'       ,'Title unknown'),
              mkl1( 'DECIMALS',0),
              mkl1( 'CREATION-DATE', format(Sys.time(), "%Y%m%d %H:%M:%S")),
              mkl1( 'LAST-UPDATED', format(Sys.time(), "%Y%m%d %H:%M:%S"))  )
    } else {  opx <- skeleton.px   }

  
 ## Add key-value pair if list.keys<>NULL
 if (! is.null(list.keys)) {    
    for (i in names(list.keys)) {
       # If there is key replaces it.
       # If it don't exist: adds new key
       if (is.null(opx[[i]])) {
               opx<-c(opx,mkl1(i, list.keys[[i]]))
       } else opx[i] <- mkl1(i, list.keys[[i]])                  
    }
  }

 # delete STUB, HEADING, VALUES and DATA if there
 opx['STUB']   <- NULL ; opx['HEADING'] <- NULL
 opx['VALUES'] <- NULL ; opx['DATA']    <- NULL  
  
 dd <- length( dim(x))
 if (dd > 1) {
     # First dim to 'header', the rest in reverse order
     opx <- c(opx,     
             mkl1( 'STUB'   , rev(names(dimnames(x))[2:dd] )),
             mkl1( 'HEADING', names(dimnames(x))[1])   )
             
     } else mkl1( 'STUB'   , rev(names(dimnames(x)) )) # Only one dim

 opx <- c( opx, list( 'VALUES' = dimnames(x)) )
 opx <- c( opx, list( 'DATA'   = list(value=as.vector(x)) ) )
 class( opx ) <- "px"
  
 return(opx)
  
}

# ### example
# oo  <- read.px(  system.file( "extdata", "example.px", package = "pxR"))
# aa  <- as.array(oo)
# as.px(aa)->px.aa1
# as.px(aa,skeleton.px=oo )->px.aa2
# as.array(px.aa1)->aa1
 
# ### export data checks
# sum(as.array(aa)-as.array(px.aa1))
# sum(as.array(aa)-as.array(px.aa2))
 
# ### Checks writing for missing data
# oo  <- read.px(  system.file( "extdata", "example2.px", package = "pxR"))
# aa  <-  as.array(oo)
# aa[sample(1:length(aa),5)]<-NA
# write.px(as.px.array(aa),file='tmpzxc01.px')

# ### append and modify keys
# write.px(as.px(aa,skeleton.px=oo),file='tmpzxc02.px')
# write.px(as.px(aa,
#              list.keys= list(MATRIX='xxx', CONTENTS='new data',
#                              NEWKEY='an other key',
#                              UNITS='people', TITLE='My Title') 
#                      ),file='tmpzxc03.px')
# 
# file.remove('tmpzxc01.px','tmpzxc02.px','tmpzxc03.px')
