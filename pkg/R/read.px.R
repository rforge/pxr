#################################################################
# 
# File:         read.px.R
# Purpose:      reads a PC-Axis file into R
#
# Created:      20110618
# Authors:      TBA
#
# Modifications: 
#
#################################################################

read.px <- function( filename ) {

    ## auxiliary functions ##

    unquote <- function( x ){
        gsub( '\\"', "", x    )
    }

    clean.spaces <- function( x ){
        x <- gsub("^[[:space:]]+","",x) # elimina espacios en blanco por delante
        x <- gsub("[[:space:]]+$","",x) # elimina espacios en blanco por detras
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


    cleanDat <- function(x){
        x <- break.clean( x, " " )
        x <- gsub( "\"..\"","NA", x )     # cambia los valores para mising ".."	## creo que es incorrecto: deber?a ser la 
        x <- gsub( "\".\"" ,"NA", x )     # cambia los valores para mising "."	## expresi?n regular "\\.\\." para el doble punto.	
                                            ## adem?s, hay que tener en cuenta que puede sustituir el punto decimal!
                                            ## igual habr?a que sustituir " \\.+ " por NA
        as.numeric( x[ x != "" ] )
    }

    make.list <- function( dat, my.label ){
        dat <- subset( dat, label == my.label, select = c( attribute, value ) )

        my.list <- as.list( dat$value )
        names( my.list ) <- dat$attribute
        my.list
    }

    ## end: auxiliary functions ##

    a <- scan( filename, what = "character", sep = "\n", quiet = TRUE )
    a <- paste( a, collapse = " " )					## " " necesario para que no junte lineas en DATA
    a <- iconv( a, "ISO_8859-1", "UTF-8")            ## just in case, on some platforms; maybe a different encoding is required
    a <- unlist( strsplit( a, ";") )
    a <- do.call( rbind, strsplit( a, "=" ) )

    a <- data.frame( cbind( get.attributes( a[, 1]), a[, 2] ) )
    colnames( a ) <- c( "label", "attribute", "value" )

    a$label     <- make.names( a$label )
    a$attribute <- make.names( a$attribute )
    a$value     <- as.character( a$value )

    ## build a px object: list with px class attribute ##

    px <- sapply( unique( a$label ), function( label ) make.list( a, label ), simplify = FALSE )


    # turns data values into an R vector
    px$DATA$value    <- cleanDat( px$DATA$value )
    px$STUB$value    <- make.names( break.clean( px$STUB$value ) )
    px$HEADING$value <- make.names( break.clean( px$HEADING$value ) )

    px$VALUES <- lapply( px$VALUES, break.clean )
    px$CODES  <- lapply( px$CODES, break.clean )

    class( px ) <- "px"
    return( px )
}

