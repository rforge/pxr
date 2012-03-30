library(pxR)


repair.codes.px <- function(x, name) {
  values <-  x$VALUES[[name]]
  codes <- x$CODES[[name]]
  if(length(values) == length(codes)) return(codes)
  else{
    mymessage <- paste("The ", name, " variable has ", length(values), " values and ", length(codes), " codes. Trying to equal both lengths.", sep ="")
   ##mymessage <- paste(mymessage,"\n", "Values:", paste(values, collapse=" "),"\n", "Codes:", paste(codes, collapse=" "),"\n")
   ## warning(message)
    if( length(values) > length(codes)) {
    ## Hay que añadir codes , ¿Cómo?
      if( length(codes) == 0) {
        ## Si no hay ningún code (el read.px elimina los "" del px),
        ## entiendo que ese code no hace referencia a ningún fichero externo
        ## por lo que podemos poner lo que queramos. En particular
        ## Corrige pcaxisedadsimplificado.px
        codes <- values
        mymessage <- paste( mymessage, "\n", "Added the following codes: ", paste(codes, collapse=" "))
        mymessage <- paste(mymessage, "\n Final result:")
        message(mymessage)
        print(data.frame(values= values, codes = codes))
        return(codes)
      }
      else {
        ## Si ya hay algún code, hay que añadir más codes.
        ## No tenemos ningún ejemplo px de este tipo de casos
        mymessage <- paste( mymessage, "\n", "We do not know how to equal both lengths")
        warning(mymessage)
        return(codes)
      }
    } else if ( length(values) < length(codes)) {
      ## Hemos de eliminar codes. ¿Cuántos?
      numberofcodestodrop <- length(codes) - length(values) 
      ## Asumo que los codes tienen un formato parecido. En particular, el mismo número de caracteres.
      ncharcodes <- nchar(codes)
      tablenchar <- table(ncharcodes)
      ## Detecto la longitud del número de caracteres de los codes que se repite tantas veces como
      ## codes que tengo que eliminar
      ncharofcodestodrop <- as.numeric(names(tablenchar[tablenchar == numberofcodestodrop]))
      ## Ahora soy bastante conservador: sólo admito esta regla cuando haya exclusivamente
      ## dos longitudes de codes y sólo haya una longitud candidata a eliminar
      if( length(ncharofcodestodrop) != 1 | length(tablenchar) != 2) {
        ## No cambio nada
       mymessage <- paste( mymessage, "\n", "We do not know how to equal both lengths")
        warning(mymessage)
        return(codes)
      }
      else {
        ## Eliminamos los codes que tienen una longitud distinta
        ## y cuyo número coincide con los que hay eliminar
        ## corrige pcaxis_mal_formado.px
        removecodes <-  codes[ which( ncharcodes == ncharofcodestodrop)]
        codes <- codes[ which( ncharcodes != ncharofcodestodrop)]
        mymessage <- paste( mymessage, "\n", "Removed the following codes: ", paste(removecodes, collapse=" "))
        mymessage <- paste(mymessage, "\n Final result:")
        message(mymessage)
        print(data.frame(values= values, codes = codes))  
        return(codes)
      }
    }
  }
}


pppx <- read.px("data/pcaxis_mal_formado.px")
name <- 'municipios'
newcodes <- repair.codes.px(pppx, name)


pppx <- read.px("data/pcaxisedadsimplificado.px")
name <- 'sexo'
newcodes <- repair.codes.px(pppx, name)



