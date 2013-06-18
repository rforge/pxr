#################################################################
# 
# File:         as.array.px.R
# Purpose:      Converts a px object to an array
#
# Created:      20110801
# Authors:      fvf
#
# Modifications: opl, fvf (20130618)
#
#################################################################

as.array.px <- function(x, use.codes = FALSE,... ){
  
   names.vals      <- c( rev(x$HEADING$value), rev( x$STUB$value ) )
    
   there.codes <- ! ( sapply(x$CODES[names.vals],is.null) )
    
   if ( is.logical( use.codes ) & use.codes == FALSE ) {
        there.codes <- rep(FALSE,length(there.codes))  # don't use codes
   } else if  ( is.character( use.codes ) ) {
                 with.codes <- ! is.na(match(names.vals,use.codes))
                 there.codes <-  with.codes & there.codes   
     } 
    
   x$VALUES[names.vals][there.codes] <- x$CODES[names.vals][there.codes] 

   if  ('KEYS' %in% names(x)) {
         dd <- pxK2df (x, use.codes) 
         result <- xtabs(dat~., data=dd)
         result[ xtabs(dat==dat~. ,data=dd ) == 0] <- NA
         attr(result,'class') <- NULL
         attr(result,'call')  <- NULL
   } else {
       result <- array( x$DATA[[1]],
                        unlist( lapply( x$VALUES[names.vals] ,length ) ),
                        dimnames = x$VALUES[names.vals] )
       names( dimnames(result) ) <- names.vals
   }
   
   return(result)
  
}


