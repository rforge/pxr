#################################################################
# 
# File:         read.px.R
# Purpose:      reads a PC-Axis file into R
#
# Created:      20110618
# Authors:      fvf, cjgb, opl
#
# Modifications: 
#		20111210, cjgb: in the data string, "-" may represent the value 0
#		20111210, cjgb: fixing the strsplit when the split character is contained in the data part
#		20120329, cjgb: number strings in the DATA part can contain ";" as separators.
#				Although deprecated, cases still lurk.
#               20130228, cjgb: There can be ; inside quoted strings that create chaos
#               20130608   fvf: Ability to read files with keys in data area.
#                               ":"  added to defaut na.string (EuroStat files)
#               20130624:  use str_split (line 91) to read DATA area
#################################################################

read.px <- function(filename, encoding = "latin1", 
                    na.strings = c('"."', '".."', '"..."', '"...."', '":"')) {

    ## auxiliary functions ##

    unquote <- function(x){
        gsub('\\"', "", x)
    }

    clean.spaces <- function(x){
        gsub("^[[:space:]]+|[[:space:]]+$", "", x) # elimina blancos por delante|detrás
    }

    get.attributes <- function(x){
        x <- gsub( "([A-Z-]*)\\((.*)\\).*", "\\1;\\2", x ) ## parte etiqueta y atributo con ";"
        x <- strsplit( x, ";" )
        x <- lapply( x, function( x ) c( x, rep( "value", 2 - length( x ) ) ) )
        x <- do.call( rbind, x )
        x[,2] <- unquote( x[,2] )
        clean.spaces( x )
    }

    break.clean <- function( x, sep = '\\"' ) {
        x <- strsplit( x, sep )[[1]]
        if (sep != " ") x <- clean.spaces( x )
        x <- x[ x != "" ]
        x <- x[ x != "," ]
        x
    }

    make.list <- function( dat, my.label ){
        dat <- subset( dat, label == my.label, select = c( attribute, value ) )

        my.list <- as.list( dat$value )
        names( my.list ) <- dat$attribute
        my.list
    }

    ## end: auxiliary functions ##

    a <- scan(filename, what = "character", sep = "\n", quiet = TRUE, fileEncoding = encoding)

    # modification by  fvf: 130608 
    # A <- PASTE(A, COLLAPSE = " ")     ## " " NECESARIO 
    
    a <- paste(a, collapse = "\n")      ## -- Se mantienen "CR/LF luego se quitaran selectivamente

    tmp <- strsplit( a, "DATA=" )[[1]]

    tmp[1] <- gsub("\n", " ", tmp[1])  # fvf[130608]: elimina CR de la cabecera
    
    tmp[2] <- gsub(";", "", tmp[2])    # removing ";" within DATA number strings
    a <- paste(tmp[1], "DATA=", tmp[2], sep = "")

    # modification by cjgb, 20130228 concerning line separators within quoted strings

    #a <- unlist(strsplit(a, ";"))	## ; is the logical line end in px files
                                        ## but there might be ; inside quoted strings;
                                        ## read.table skips them efficiently, therefore:

    punto.coma <- str_locate_all(a, ";")[[1]][,1]	# where the ";" are
    comillas   <- str_locate_all(a, '"')[[1]][,1]	# where the '"' are
    cortes     <- Filter( function(x) sum(comillas < x) %% 2 == 0, punto.coma )		# ";" not after an odd number of '"'
                                                                                        # these are the proper "cuts"

    a <- str_sub(a, c(1, cortes + 1), c(cortes - 1, str_length(a)))

    a <- a[!is.na(a)]
    a <- a[a != ""]
   
    a <- sub( "=", "//=//", a )
    a <- do.call(rbind, str_split(a, "//=//" ))     # change strsplit by str-split. In big px-files:
                                                    #  "Error: C stack usage is too close to the limit"
    a <- data.frame(cbind(get.attributes(a[, 1]), a[, 2]))
    colnames(a) <- c("label", "attribute", "value")

    a$label     <- make.names(a$label)
    a$attribute <- make.names(a$attribute)
    a$value     <- as.character(a$value)
    
    ## build a px object: list with px class attribute ##

    px <- sapply(unique( a$label ), function(label) make.list(a, label), simplify = FALSE)

    # turns data values into an R vector
    px$STUB$value    <- make.names(break.clean(px$STUB$value))
    px$HEADING$value <- make.names(break.clean(px$HEADING$value))

    px$VALUES <- lapply(px$VALUES, break.clean )
    px$CODES  <- lapply(px$CODES,  break.clean )

    tmp <- gsub('"-"', 0, px$DATA$value)        # 0 can be encoded as "-"

    # fvf[130608]: add to to read files with keys in data area 
    if ('KEYS' %in% a$label )    # Are There KEYS DATA$values ? 
       { dat <- textConnection(tmp)
         names(px$KEYS) -> names.keys
         sapply(px$KEYS, function(e) {e} ) -> cont.keys
         no.keys <- names(px$VALUES)[-match(names(px$KEYS),names(px$VALUES))]
         c(lapply(names.keys,c),'')-> lista
         names(lista)<- c(names.keys,'datanum')
         scan(dat,what=lista,sep=',') -> pp
         lapply(pp$datanum, function(e)
                             scan(text=e,na.strings=':',quiet=T) )-> pp$datanum
         px$DATA$value <- pp
         grid.keys<- px$VALUES[no.keys]
         grid.keys <-expand.grid(rev(grid.keys))
         grid.keys[,rev(names(grid.keys)),drop=FALSE] -> px$DATA$datakeys
         close(dat)
       }  else
       {
         gsub("\n", " ", tmp)->tmp    # delete CR/LF of DATA area fvf[130608]
         # dat <- textConnection(px$DATA$value) #much faster than with cleanDat (strsplit)
         dat <- textConnection(tmp)
         px$DATA$value <- scan(dat, na.strings = na.strings, quiet = TRUE)
         close(dat)
       }
           
    class(px) <- "px"
    px
}

