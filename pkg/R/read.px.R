#################################################################
# 
# File:         read.px.R
# Purpose:      reads a PC-Axis file into R
#
# Created:      20110618
# Authors:      fvf, cjgb, opl
#
# Modifications: 
#
#################################################################

read.px <- function( filename , encod.from = "ISO_8859-1", encod.to = "UTF-8") {

    ## auxiliary functions ##

    unquote <- function( x ){
        gsub( '\\"', "", x    )
    }

    clean.spaces <- function( x ){
        x <- gsub( "^[[:space:]]+", "", x ) # elimina espacios en blanco por delante
        x <- gsub( "[[:space:]]+$", "", x ) # elimina espacios en blanco por detras
        x
    }

    get.attributes <- function( x ){
        x <- gsub( "([A-Z-]*)\\((.*)\\).*", "\\1;\\2", x ) ## parte etiqueta y atributo con ";"
        x <- strsplit( x, ";" )
        x <- lapply( x, function( x ) c( x, rep( "value", 2 - length( x ) ) ) )
        x <- do.call( rbind, x )
        x[,2] <- unquote( x[,2] )
        clean.spaces( x )
    }

    break.clean <- function( x, sep = '\\"' ) {
        #x <- strsplit( unquote( x ), sep )[[1]]

        x <- strsplit( x, sep )[[1]]
        x <- clean.spaces( x ) 
        x <- x[ x != "" ]
        x <- x[ x != "," ]
        x
    }


    cleanDat <- function( x ){
        x <- break.clean( x, " " )
        x <- gsub( ".*,", "", x )           # eliminates keys part in files with KEYS argument
        x <- gsub( "\"\\.\\.\"","NA", x )
        x <- gsub( "\"\\.\"" ,"NA", x ) 
        as.numeric( x[ x != "" ] )
    }

    get.keys <- function( x, codes, values, keys ){
        x <- break.clean( x, " " )
        x <- x[ grep( ",", x ) ]                # keep just components having a comma 
        x <- unquote( x )
        x <- do.call( rbind, sapply( x, break.clean, ",", simplify = F ) )
        x <- data.frame( x[, -ncol( x ) ] )               # drops the data part
        rownames( x ) <- NULL
        colnames( x ) <- rev( names( keys ) )
        
        encoded.vars <- names( keys )[ keys == "CODES" ]
        for( var.name in encoded.vars ){
            x[[var.name]] <- values[[ var.name ]] [ match( x[[ var.name ]], codes[[ var.name ]] ) ]
        }
        x
    }

    make.list <- function( dat, my.label ){
        dat <- subset( dat, label == my.label, select = c( attribute, value ) )

        my.list <- as.list( dat$value )
        names( my.list ) <- dat$attribute
        my.list
    }

    ## end: auxiliary functions ##

    a <- scan( filename, what = "character", sep = "\n", quiet = TRUE )
    a <- paste( a, collapse = " " )	## " " necesario para que no
                                        ## junte lineas en DATA
    a <- iconv( a, from=encod.from, to=encod.to)    ## just in case, on some
                                             ## platforms; maybe a
                                             ## different encoding is
                                             ## required
    a <- unlist( strsplit( a, ";", useBytes = T ) )
    a <- do.call( rbind, strsplit( a, "=" ) )

    a <- data.frame( cbind( get.attributes( a[, 1]), a[, 2] ) )
    colnames( a ) <- c( "label", "attribute", "value" )

    a$label     <- make.names( a$label )
    a$attribute <- make.names( a$attribute )
    a$value     <- as.character( a$value )

    ## build a px object: list with px class attribute ##

    px <- sapply( unique( a$label ), function( label ) make.list( a, label ), simplify = FALSE )

    # turns data values into an R vector
    px$STUB$value    <- make.names( break.clean( px$STUB$value ) )
    px$HEADING$value <- make.names( break.clean( px$HEADING$value ) )

    px$VALUES <- lapply( px$VALUES, break.clean )
    px$CODES  <- lapply( px$CODES, break.clean )

    # only if data contains the KEYS keyword
    # such datasets have a different data format and require an extra field to
    # keep track of which rows are present in the data (see format document for details)
    #if( "KEYS" %in% names( px ) )
    #    px$internal.keys <- get.keys( px$DATA$value, px$CODES, px$VALUES, px$KEYS )

    px$DATA$value    <- cleanDat( px$DATA$value )
    class( px ) <- "px"
    return( px )
}

