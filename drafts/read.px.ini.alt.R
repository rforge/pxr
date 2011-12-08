read.pxini <- function(filename, shpPath = 'shp',  encoding = "latin1", ...){

    if( ! grepl(".ini$", filename) ) 
        filename <- paste(filename, ".ini", sep = "")

    filename <- file.path( shpPath, filename )      # Platform independent path construction

    a <- scan(filename, what = "character", sep = "\n",
                quiet = TRUE, fileEncoding = encoding)

    foo <- function( pattern, strings ){
        tmp <- strings[grep(pattern, strings, ignore.case = T)]
        gsub(pattern, "", tmp)
    }

    filesshp  <- foo("^[0-9]*=", a)
    filesshp  <- gsub(".*\\\\", "", filesshp)
    filesshp  <- file.path(shpPath, filesshp)

    list( 
            nummaps   = as.numeric(foo("NumMaps=", a)),
            filesshp  = foo("KeyField=", a),
            keyfields = keyfields
    )
}


## filename <- "spain_ccaapro_img_ind"

## read.pxini(filename=filename)
