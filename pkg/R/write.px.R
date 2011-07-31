#################################################################
# 
# File:         write.px.R
# Purpose:      Write an object of class 'px' to a PC-Axis file 
#
# Created:      20110618
# Authors:      fvf, cjgb, opl
#
# Modifications: 
#
#################################################################


write.px <- function ( obj.px, filename, encod = "ISO_8859-1" )
{

 if ( ! inherits( obj.px, "px" ) )
   stop('Error: object does not have the right "class"')
 
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
         # Key without  variable name 
         zz <- paste(obj.px[[i]]$value,collapse='","')
         zz <- unquote(zz)         
         if (i %in% c('DECIMALS','SHOWDECIMALS','ELIMINATION')) # values without quote
                                                       # falla lectura PC-AXIS con comillas
          {      wf( c(i,'=' , zz ,';\n')  )        
          } else wf( c(i,'="', zz ,'";\n'))          
         } else # meta with second name. Can be more one
         { for (l in names(obj.px[[i]]))
            {
              if (i %in% c('KEYS')) # values without quote
                {      wf ( c(i,'("',l,'")=') )
                } else wf ( c(i,'("',l,'")=\"') )              
              wf ( paste(obj.px[[i]][[l]],collapse='","') )
              if (i %in% c('KEYS')) # values without quote
              { wf ( ';\n' ) } else wf ( '";\n' )              
            }
         }     
    }
  # write DATA
  zz <- as.array( obj.px )
  ### Innecesario ---
  # n  <- length( length(dim(zz)) ) # Nunca mayor de 2
  # if ( n>2 ) # -- change first to second dim
  # {
  #   dd<-(1:n) ;  dd[1:2]<-dd[c(2:1)]
  #  aperm(zz,dd)->zz
  # }
  wf ('DATA=\n')
  ## Hay que multiplicar po 10^DECIMAL ??? 
  formatC(zz)->zz
  zz[zz=='NA']<-'".."'
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
