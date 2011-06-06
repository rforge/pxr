####################################################################
px.write.ob<- function (data,file="",ANSI=TRUE,
                        decimals=2,format="f")
#  Escribe un objeto de tipo px.
#  Con los datos en px$DATOS,
#   [1] "SUBJECT-AREA" "SUBJECT-CODE" "MATRIX"       "TITLE"
#   [5] "CONTENTS"     "UNITS"        "DATA"         "HEADING"
#   [9] "STUB"

{
  px.write(data,
          file=file,
          title=data$TITLE,
          contens=data$CONTENTS,
          units=data$UNITS,
          subjectarea=data$SUBJECT-AREA,
          subjectcode=data$SUBJECT-CODE,
          decimals=decimals,format=format,
          ANSI=ANSI )
 }

#################################
#################################
write.px<- function (x,
                     file="",
                     title="Sin titulo",
                     contens="tipo contenido",
                     units="cosas",
                     subjectarea="00",
                     subjectcode="xx",
                     showdecimals=5,
                     decimals=2,format="f",
                     ANSI=TRUE)
# Funcion Exporta a pcaxis: px
# la 1º dimensión colum se emplea como variable desplegada
# se le suministra la array y el nombre del fichero
# usa la lista  "dimnames" para etiquetar de las categorias
# usa los names(dimnames) como nombres de las variables
{
  # -------------------------------------------------
  # -- cambia 1º por 2º dim
  dc<-attr(x,"dimcodes")
  dd<-1:length(dim(x)) ;  dd[1:2]<-dd[c(2:1)]
  aperm(x,dd)->x
  # -------------------------------------------------
  cat(paste("CHARSET=\"",ifelse(ANSI,"ANSI","OEM"), "\";\n",sep=""),file=file)
  cat("AXIS-VERSION=\"2000\";\n",file=file,append=T,sep="")
  cat("SUBJECT-AREA=\"",subjectarea,"\";\n",file=file,append=T,sep="")
  cat("SUBJECT-CODE=\"",subjectcode,"\";\n",file=file,append=T,sep="")
  cat(paste("MATRIX=\"",  strsplit(basename(file),"\\.")[[1]][1],"\";\n",sep=""),file=file,append=T)
  cat(paste("TITLE=\"",title,"\"\n\"por ",paste(names(dimnames(x)),collapse = ", "),"\";\n",
            sep=""),file=file,append=T)
  cat(paste("CONTENTS=\"",contens,"\";\n",sep=""),file=file,append=T)
  cat(paste("STUB=\"",paste(rev(names(dimnames(x))[-1]),collapse="\",\""),"\";\n",sep=""),file=file,append=T)
  cat(paste("HEADING=\"",names(dimnames(x))[1],"\";\n",sep=""),
      file=file,append=T)

  for (n in (length(dim(x))):1) {
     cat(
     paste("VALUES","(\"",names(dimnames(x))[n],"\")=\"",
           paste(dimnames(x)[[n]],collapse="\",\"",sep=""),"\";\n",sep="")
     ,file=file,append=T)
  }
  cat(paste("UNITS=\"", units,"\";\n",sep=""),file=file,append=T)
  if( !is.null(dc)) {
    for (n in (length(dim(x))):1) {
     cat(
     paste("CODES","(\"",names(dc)[n],"\")=\"",
           paste(dc[[n]],collapse="\",\"",sep=""),"\";\n",sep="")
     ,file=file,append=T)
    }
  }
  cat(paste("DECIMALS=",decimals,";\n",sep=""),file=file,append=T)
  cat(paste("SHOWDECIMALS=",showdecimals,";\n",sep=""),file=file,append=T)
  cat("DATA=\n",file=file,append=T)
  # ancho=trunc(log10(max(x)))+decimals+1
  # , width=ancho
  x<-round(x,digits=decimals)
  # x<-(forma(x)
  # x<-formatC(x,digits=decimals,format=format)
  write(x,
                file=file,ncolumns=dim(x)[1],append=T)
  cat(";",file=file,append=T)
}
