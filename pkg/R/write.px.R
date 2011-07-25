#################################################################
# 
# File:         read.px.R
# Purpose:       Write  a objest of class 'px' to a PC-Axis file 
#
# Created:      20110618
# Authors:      fvf, cjgb, opl
#
# Modifications: 
#
#################################################################


write.px<- function (obj.px , filename , encod="ISO_8859-1")
{

 if (class(obj.px)[1] != 'px')
   { return('Error: object does not have the right "class"') }
 
  ## auxiliary functions ## 
  unquote <- function( x ){
        gsub( '^"', "", x    )->x
        gsub( '"$', "", x    )
  }
  
  con <- file( description = filename, open = "w", encoding = encod )

 wf  <- function ( lchart ) {
          cat(paste(lchart,sep='', collapse =''), file=con) }
  
  # modify px objects 
  obj.px[['LAST.UPDATED']]$value <- 
       format(Sys.time(), "%Y%m%d %H:%M:%S")
  
  obj.px[['CHARSET']]$value <- ifelse( encod == "ISO_8859-1", 'ANSI',encod )

  # write new file 
  cat('',file = con,append = F) # init file
  
  lapply(obj.px,names) -> lcell
  for (i in  names(lcell)[names(lcell) != 'DATA'])
    {
      if (length(i)==1 & lcell[[i]][1] == 'value') {
         # field meta without second name lees DATA 
         zz <- paste(obj.px[[i]]$value,collapse='","')
         zz <- unquote(zz)
         wf(c(i,'="',zz,'";\n'))
         } else # meta with second name. Can be more one
         { for (l in names(obj.px[[i]]))
            {
              wf ( c(i,'("',l,'")=\"') )
              wf ( paste(obj.px[[i]][[l]],collapse='","') )
              wf ( '";\n' )
            }
         }     
    }
  # write DATA
  zz <- as.array( obj.px )
  n  <- length( length(dim(zz)) )
  if ( n>2 ) # -- change first to second dim
  {
    dd<-(1:n) ;  dd[1:2]<-dd[c(2:1)]
    aperm(zz,dd)->zz
  }
  wf ('DATA=\n')
  write ( zz, file=con, ncolumns=dim(zz)[1], append=T )
  cat   (";", file=con, append=T)
  close ( con )
}  


### Example

# opx1 <- read.px(  system.file( "extdata", "example.px", package = "pxR")  )  
# write.px ( opx1, file = 'opx.px')  #  write a   copy
# opx2 <- read.px  ('opx.px')        #  read  the copy

### are de same data ?
# as.array(opx1)-> a1
# as.array(opx2)-> a2
# sum(a1-a2)
