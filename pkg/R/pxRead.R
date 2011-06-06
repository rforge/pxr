read.px <- function (file, sourceEncoding='CP437', targetEncoding='latin1')
#   Lee un fichero de pc-axis: "*.px"
#   y lo introduce en una estructura de tipos px
#   --------- Definición de un objeto de tipo "px"-----------------
#     px$data -> contiene la matriz de datos con nombres de las
#     variables y categorias
#     px$var  -> lista con nombres de variables
{
  # file="41.px"
  ## Declara variable a usar en la rutina  --
  px   <-list()   ## almacena  el objeto tipo px, que devolvera la rutina
  zz   <-NULL   ## Puntero de fichero
  px.c <-NULL   ## vector con lineas de cabecera de fichero px
  lp   <-""     ## varible con linea previa
  nl   <-0      ## contador del num de lineas
  NamesVar=c()  ## Contendra los nombres de las varibles
  a    <-NULL   ## Variable de trabajo almacena array
  ## ----
  zz<-file(file,"rt")
  # Lee cabecera del fichero px: Hasta linea de DATA=
  px.c<-c() # Contiene la cabecera del fichero "px"
  px.dat<-c() # Contiene la parte con los datos del fichero "px"
  is.ansi<-FALSE  # si formato es ANSI

  while ( strsplit(l<-readLines(zz,1),"=")[[1]][1]!="DATA" ) {
        # Comprueba que esta linea termina en ";"
        gsub(";[[:space:]]+$",";",l)->l   # quita espacioe blancos por detras de ";"    
	nl<-nl+1
	if (substr(l,nchar(l),nchar(l))==";") {
		 lp<-paste(lp,substr(l,1,nchar(l)-1),sep="")
		 px.c<-c(px.c,lp)
		 lp<-"" }
        else lp<-paste(lp,l,sep="")
    }

  ## Lineas de Interes: STUB, HEADING,VALUES, CODES, ANSI
  # 1.- separa por "="
  gsub("^[[:space:]]+","",px.c)->px.c       # eliminia espacios en blanco por delante
  gsub("[[:space:]]+$","",px.c)->px.c       # eliminia espacios en blanco por detras
  for (i in px.c) {
     strsplit(i,"=")[[1]][1]->cab
     strsplit(i,"=")[[1]][2]->col
     if (cab=="ANSI") {
     	is.ansi<-TRUE  # si formato es ANSI
     }
     if (cab=="STUB") {
     	NamesVar<-c(NamesVar,rev(gsub("\"","",strsplit(col,"\", +|\",")[[1]])))
        }
     else if (cab=="HEADING") {
     	NamesVar<-c(rev(gsub("\"","",strsplit(col,"\", +|\",")[[1]])),NamesVar)
        }
     ## Almacena en objeto las siguientes conceptos
     lw<-c("SUBJECT-AREA","SUBJECT-CODE","MATRIX","TITLE","CONTENTS","UNITS")
     if (length(grep(paste("^",cab,sep=""),lw))>0) {
     	px[[cab]]=gsub("\"","",col)
        }
  }
   ##oem2ansi(NamesVar)->NamesVar
  NamesVar <- iconv(NamesVar, sourceEncoding, targetEncoding)
  ## for (i in 1:length(px)) { if (!is.ansi) { px[[i]]<-oem2ansi(px[[i]]) }}
  ## for (i in 1:length(px.c)) if (!is.ansi) { px.c[[i]]<-oem2ansi(px.c[[i]]) }
  for (i in 1:length(px)) { if (!is.ansi) { px[[i]]<-iconv(px[[i]], sourceEncoding, targetEncoding)}}
  for (i in 1:length(px.c)) if (!is.ansi) { px.c[[i]]<-iconv(px.c[[i]], sourceEncoding, targetEncoding) }
  
  # ... gsub("\"","",NamesVar)->NamesVar
  # Variables a incluir en la matriz
  listcat=list()  # Contendra las categorias de cada variable
  listcod=list()  # Contendra los codigos de cada variable
  for (i in NamesVar) {
    listcat[[i]]<-c(NA)
    listcod[[i]]<-c(NA)
  }
  # Rellena las categorias de cada variable
  for (i in px.c) {
     strsplit(i,"=")[[1]][1]->cab
     strsplit(i,"=")[[1]][2]->col
     if (substr(cab,1,6)=="VALUES") {
     	 var<-gsub("\"","",strsplit(cab,"\\(\"|\"\\)")[[1]][2])
     	 val<-gsub("\"","",strsplit(col,"\", +|\",")[[1]])
     	 listcat[[var]]<-val
     }
     if (substr(cab,1,5)=="CODES") {
     	 var<-gsub("\"","",strsplit(cab,"\\(\"|\"\\)")[[1]][2])
     	 val<-gsub("\"","",strsplit(col,"\", +|\",")[[1]])
     	 listcod[[var]]<-val
     }
  }
  # attributes(listcat)

  # a<-scan(pipe(paste("d:/ut/gawk   \"{gsub(/;/,NADA); if(length($0)>2) print}\"", file)),
  #        skip = nl+1, quiet= TRUE,na.strings =c("\"..\"",";"),dec=".",sep="")

  # continua leyendo a partir de DATA=
  px.dat<-readLines(zz,-1,ok=TRUE)
  gsub("DATA=","",l)->l  # Recupera datos en la linea DATA=
  px.dat<-paste(l,px.dat,sep=' ')
  gsub(";","",px.dat)->px.dat  # elimina los caracteres ";"
  px.dat[px.dat!=""]->px.dat   # quita posible lineas en blanco
  paste(px.dat,  collapse =" " )->px.dat
  gsub("\"..\"","NA",px.dat)->px.dat  # cambia los valores para mising ".."
  gsub("\".\"","NA",px.dat)->px.dat   # cambia los valores para mising "."
  gsub("[[:space:]]+"," ",px.dat)->px.dat       # elimina dobles espacios 
  gsub("^[[:space:]]+","",px.dat)->px.dat       # eliminia espacios en blanco por delante
  gsub("[[:space:]]+$","",px.dat)->px.dat       # eliminia espacios en blanco por detras
  as.numeric(strsplit(px.dat," ")[[1]])->px.dat

  # Crea array receptor
  a<-array(px.dat,unlist(lapply(listcat,length)),dimnames=listcat)
  a<-aperm(a,length(dim(a)):1)
  # browser() #-------------------------
  px$DATA<-a
  px$HEADING<-NamesVar[1]
  px$STUB<-NamesVar[-1]
  # -- cambia 1. por 2. dim
  dd<-rev(1:length(dim(px$DATA))) ;  dd[1:2]<-dd[c(2:1)]
  aperm(px$DATA,dd)->px$DATA
  if (!is.null(listcod)) {
     dc<-dimnames(px$DATA)
     for (i in names(dc)) dc[[i]]<-listcod[[i]]
     attr(px$DATA,"dimcodes")<-dc
  }
  close(zz,"rt")  # ---- cierra el fichero

  px
}
