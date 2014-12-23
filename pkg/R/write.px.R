#################################################################
# 
# File:         write.px.R
# Purpose:      Write an object of class 'px' to a PC-Axis file 
#
# Created:      20110618
# Authors:      fvf, cjgb, opl
#
# Modifications: 
#               fvf (20130618)
#               cjgb, 20130811: added on.exit hook to close the open connection
#               cjgb, 20130811: fixed encoding issues (testing)
#               cjgb, 20130813: we do not generate files using KEYS (i.e., if present,
#                         the KEYS part needs to be ignored)
#               cjgb, 20131117: some users want to specify heading & stub precisely
#                         in order to control the shape of the resulting matrix
#               fvf,  20141222: Allows use of "KEYS" for the export of sparse matrices.
#                               Use CODES values, if any, to identify rows.
#                               Fixed a bug related to iconv
#################################################################


write.px <- function ( obj.px, filename, heading = NULL, stub = NULL,
                       keys = NULL , write.na = FALSE, write.zero = FALSE ,
                       fileEncoding = "ISO-8859-1" )
{
  
  if ( ! inherits( obj.px, "px" ) )
    stop("Error: object needs to have class 'px'")
  
  ## auxiliary functions ## 
  unquote <- function(x)
    gsub( '^"|"$', "", x)           # removes " at beginning/end of string
  
  requote <- function(x)            # adds dquotes (") to character vectors
    paste( '"', unquote(x), '"', sep = "")
    
  wf  <- function ( ..., my.encoding = ifelse(fileEncoding == "ISO-8859-1", "latin1", fileEncoding) ) {
    cadena <- paste(..., sep = "")
    cadena <- iconv(cadena, to=my.encoding)
    cat( cadena, file = con, sep = "" ) 
  }
  ## end - auxiliary functions ##
  
  # modify px object providing sensible default values
  obj.px[['LAST.UPDATED']]$value <- format(Sys.time(), "%Y%m%d %H:%M:%S")
  obj.px[['CHARSET']]$value      <- ifelse( fileEncoding == "ISO-8859-1", 'ANSI', fileEncoding )
  obj.px$INFO$value              <- "File generated using R and package pxR (http://pxr.r-forge.r-project.org/)"

  # Using KEYS Allowed: as parameter or object property

  if (!is.null(keys)) {
    obj.px$KEYS <- NULL  ## Redefine KEYS si existen
                         ## CODES si hay, si no VALUES
    if ( ! all(keys %in% names(obj.px$VALUES)))    {
        c('!! Error Somme keys are not in VALUES !! ')
    }
    
    
    lapply(keys,function(e) { 'VALUES' })-> kk ;  names(kk)<- keys    
    for (i in keys[keys %in% names(obj.px$COD)]) {
      kk[[i]]<- 'CODES'
      levels(obj.px$DATA[[1]][,i]) <- obj.px$CODES[[i]]
      ## Change by defaut ("levels"): VALUES by CODES: 
    }    
    obj.px$KEYS <- kk
  }
  
  # CREATION-DATE:
  
  if  ( is.null(obj.px$'CREATION-DATE')  | is.null(obj.px$'CREATION.DATE') ) {
    obj.px$'CREATION-DATE'$value <- format(Sys.time(),'%Y%m%d %H:%m')
  }
  
  
  # obj.px names may have changed (and got - changed into . by R)
  # we need to revert that  
  names(obj.px) <- gsub("\\.", "-", names(obj.px))
  
  # we want to write the output file keywords in the 'right' order
 
  order.kw <- c("CHARSET", "AXIS-VERSION", "CODEPAGE", "LANGUAGE",
                "LANGUAGES", "CREATION-DATE", "NEXT-UPDATE", "PX-SERVER",
                "DIRECTORY-PATH", "UPDATE-FREQUENCY", "TABLEID", "SYNONYMS",
                "DEFAULT-GRAPH", "DECIMALS", "SHOWDECIMALS", "ROUNDING",
                "MATRIX", "AGGREGALLOWED", "AUTOPEN", "SUBJECT-CODE",
                "SUBJECT-AREA", "CONFIDENTIAL", "COPYRIGHT", "DESCRIPTION",
                "TITLE", "DESCRIPTIONDEFAULT", "CONTENTS", "UNITS", "STUB",
                "HEADING", "CONTVARIABLE", "VALUES", "TIMEVAL", "CODES",
                "DOUBLECOLUMN", "PRESTEXT", "DOMAIN", "VARIABLE-TYPE",
                "HIERARCHIES", "HIERARCHYLEVELS", "HIERARCHYLEVELSOPEN",
                "HIERARCHYNAMES", "MAP", "PARTITIONED", "ELIMINATION", "PR",
                "ECISION", "LAST-UPDATED", "STOCKFA", "CFPRICES", "DAYADJ",
                "SEASADJ",  "CONTACT", "REFPERIOD", "BASEPERIOD",
                "DATABASE", "SOURCE", "SURVEY", "LINK", "INFOFILE",
                "FIRST-PUBLISHED", "META-ID", "OFFICIAL-STATISTICS", "INFO",
                "NOTEX", "NOTE", "VALUENOTEX", "VALUENOTE", "CELLNOTEX",
                "CELLNOTE", "DATASYMBOL1", "DATASYMBOL2", "DATASYM", "BOL3",
                "DATASYMBOL4", "DATASYMBOL5", "DATASYMBOL6", "DATASYMBOLSUM",
                "DATASYMBOLNIL", "DATANOTECELL", "DATANOTESUM", "DATANOTE",
                "KEYS", "ATTRIBUTE-ID", "ATTRIBUTE-TEXT", "ATTRIBUTES",
                "PRECISION","DATA")
 
  order.px  <- charmatch(names(obj.px), order.kw, nomatch=999)   # keyword no in list to end
  new.order <- setdiff( names(obj.px)[order(order.px)], "DATA" ) # all but "DATA"
  
  if(! is.null(heading)){
    if(is.null(stub))
      stop("If heading is specified, you need also specify the stub parameter.")
    
    if(! setequal(c(heading, stub), c(obj.px$HEADING$value, obj.px$STUB$value)) )
      stop("Specified heading and stub parameters differ from those in the px object")
    
    obj.px$HEADING$value <- heading
    obj.px$STUB$value    <- stub
  }

  # If thera are key:"KEYS"  then 
  # change HEADING and STUB to fit KEYS

   if (! is.null(obj.px$KEYS)) {
        
    keys    <- names(obj.px$KEYS)
    values  <- names(obj.px$VALUES)    
    no.keys <- values[! (values %in% keys) ]
    
    obj.px$STUB$value     <- keys
    obj.px$HEADING$value  <- no.keys
  }
 
  ## open the connection and close automatically on exit
  con <- file( description = filename, open = "w", encoding = fileEncoding )
  on.exit(close(con))
  
  ## write new file 
  #cat('', file = con, append = F) # init file
  
  ## metadata part
  for (key in new.order ) {

    if (length(obj.px[[key]]) == 0)
      next				# this fixes a bug where length(CODES) == 0    
    
    # these are exceptions: no quotes
    # e.g.: 'DECIMALS=0;'
     ## ELIMINATION("~~~~")=YES is a diferente exceptions fvf: (20141222)
    
    if( key %in% c('DECIMALS', 'SHOWDECIMALS', 
                   'COPYRIGHT', 'DESCRIPTIONDEFAULT', 'DAYADJ', 'SEASADJ')){
      wf( key, "=")
      wf( unquote(obj.px[[key]]$value) )
      wf(';\n')
      next
    }
    
    # most metadata entries are like this: 
    # 'KEY="a","b";'
    if ( names(obj.px[[key]])[1] == 'value' ) {  
      wf( key, "=")
      wf( paste( requote(obj.px[[key]]$value), collapse = ',') )
      wf(';\n')
      next
    } 
    
    # meta with second name; there can be more than one
    for (subkey in names(obj.px[[key]])){
      wf ( key, '("', subkey, '")=' )
      # ELIMINATION is here: fvf (20141222)
      if   (key =='ELIMINATION') {
         if (obj.px$ELIMINATION[[subkey]] %in% c('YES','NO')) 
            wf(obj.px$ELIMINATION[[subkey]]) 
         else
           wf ( paste( requote( obj.px[[key]][[subkey]] ), collapse = ',') ) 
      } else if  (key =='KEYS' )   wf( paste( obj.px[[key]][[subkey]],';',sep=''))
      else  wf ( paste( requote( obj.px[[key]][[subkey]] ), collapse = ',') )
      wf ( ';\n' )              
    }     
  }
 
  # DATA part:  KEYS are allowed 20141222 -------------------------------
  
  wf('DATA=\n')
  
  if (! is.null(obj.px$KEYS)) {
    
     keys   <- names(obj.px$KEYS)
     values <- names(obj.px$VALUES)
     
     fm <- formula( paste(  
              paste(keys,collapse = '+'),'~', 
              paste(values[!values %in% keys],collapse = '+'),sep='') )    
    
     #  levels KEYS to CODES or VALUES:
     for (i in keys) {
       if (obj.px$KEYS[[i]]=='CODES') {
         levels(obj.px$DATA[[1]][,i]) <- obj.px$CODES[[i]] 
       } else 
         levels(obj.px$DATA[[1]][,i]) <- obj.px$VALUES[[i]]        
     }
          
     
     obj.px$DATA[[1]] <- dcast(obj.px$DATA[[1]],fm,sum)   # reciclo memory
     
     with.data <- rep(TRUE,length(obj.px$DATA[[1]][,1]))
     
     no.keys <- names(obj.px$DATA[[1]])[!names(obj.px$DATA[[1]]) %in% keys] 
     
     if ( ! write.na) {
       
         kk2 <- as.matrix(obj.px$DATA[[1]][,no.keys])
         kk2[] <-  ! is.na(kk2[])
         
         with.data <-  rowSums(kk2) > 0 # delete if all NA
         
     }
     if ( ! write.zero) {
       
        kk2 <- as.matrix(obj.px$DATA[[1]][,no.keys])
        kk2[] <-  (!kk2[]==0)
       
        with.data <-  rowSums(kk2)  > 0 # delete if all cero
     }
     
     if ( (! write.na) & (! write.zero) ) {
       kk2 <- as.matrix(obj.px$DATA[[1]][,no.keys])
       kk2[] <-  ! (is.na(kk2[]) |  kk2[]==0 )   
       
       with.data <-  rowSums(kk2)  > 0 # delete if all cero
       
     }
     
     obj.px$DATA[[1]] <- obj.px$DATA[[1]][with.data, ]    # delete Rows without data
     
     rm(kk2)                                              # preserve memory
     
     zz<- obj.px$DATA[[1]][,keys[1]] 
     
     
     
     for (i in keys[-1]) {
       zz<-paste(zz,obj.px$DATA[[1]][,i],sep='","')
       
     }
     
     zz <- paste('"',zz,'",',sep='')
     for (i in names(obj.px$DATA[[1]])[!names(obj.px$DATA[[1]]) %in% keys] ) {
        colum.num <- formatC(obj.px$DATA[[1]][,i],
                        format = 'f',
                        digits = as.numeric(obj.px$DECIMALS$value),
                        drop0trailing = T, flag = '-')
        colum.num <- gsub("NA", '".."', colum.num)        
        zz<-paste(zz,colum.num,sep=' ')
        
     }
    
    
     write(zz, file = con, ncolumns = 1, append = T )
     
  } else {
      zz <- formatC(as.array(obj.px),
                  format = 'f',
                  digits = as.numeric(obj.px$DECIMALS$value),
                  drop0trailing = T, flag = '-')
      #zz <- str_trim(zz) 
      zz <- gsub("NA", '".."', zz)  
      zz <- aperm(zz, c(rev(obj.px$HEADING$value), rev(obj.px$STUB$value)))
      write(zz, file = con, ncolumns = sum( dim(zz)[1:length(obj.px$HEADING$value)] ), append = T )
  }
  
  wf(";\n")
  
  return(paste(filename,'was created'))
  
}  

