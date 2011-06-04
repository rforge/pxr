px.agr<-function(strURL,newDim)
# strURL es la localizacion del fichero tipo px.
# newDim es una lista con la correspondecia entre las antiguas y la nuevas dimensiones
#
{  tf<-tempfile(pattern = "__")
   intentos<-0 ; ee<-0 ; class(ee)<-"try-error" ## Intenta alguna veces bajar fichero
   while (class(ee)=="try-error" && intentos<10) {
      intentos<-intentos+1 ; print (paste("INTENTO:",intentos))
      try(download.file(strURL,tf))->ee
   }
   A<-px.read(tf)$DATA
   A<-mdim.agr(A,newDim,fun="sum")
   return(A)
}

mdim.agr<-function(A,newDim,fun="sum")
#  ---- rutina general de AGREGRACION MULTIDIMENSIONAL de array
# A es una array multidimensional con attributos "dimnames"
# newDim es una lista con el mismo numero de elementos que "dimnames(A)"
# pero con elementos repetidos (aquellos que se agrega, si fun=sum)
# o con elementos en NA, que seran aquellos que se eliminan
{
   dimnames(A)->oldDim
   # comprueba que el n de dimensiones coinciden
   if (length(oldDim)!=length(newDim)) stop("ERROR ..no coinciden las tablas")
   nDim<-length(dim(A))
   dimA<-dimA<-1:nDim
   for (i in dimA<-(1:length(dim(A)))) {
        sel<-newDim[[i]]
        apply(A,dimA[-i],function(e)
            { tapply(e,sel,fun,na.rm=T, simplify = TRUE)[unique(sel[!is.na(sel)])]})->AA
        # se producira un error si el numero de categorias agregadas es "1",
        # por reduccion de las dimensiones
        unique(sel[!is.na(sel)])->cc
        if (length(cc)==1) # en ese caso añadir una dimensión al principio
        { dn<-dimnames(AA)
          AA<-array(AA,c(1,dim(AA)))
          dimnames(AA)<-c(cc,dn)
          names(dimnames(AA))[1]<-names(oldDim)[i]
        }
        dimP<-order(c(i,dimA[-i]))
        aperm(AA,dimP)->A
   }
   return(A)
}
#  A<-array(1:24,c(4,3,2))
#  dimnames(A)<-list(d1=1:4,d2=1:3,d3=1:2)
#  newDim<-dimnames(A)
#  newDim[[1]]<-c("a1","a2","a3","a3") # sin NA
#  newDim[[2]]<-c("x1",NA,"x1")
#  mdim.agr(A,newDim)->AA
#  mbind(2,AA,array(NA,c(4,1,2),dimnames=list(d1=1:4,d2="-",d3=1:2)),A)


#########################################################################
pad<-function(x,n=3,c="0")
# Ajusta por la izquierda un n de posiones
# rellenando con el caracter indicado
        {
        if (is.numeric(x)) x<-format(x,trim=T)
	x<-paste(paste(rep("0",n),sep="",collapse = ""),x,sep="")
	nn<-nchar(x)
	x<-substr(x,nn-n+1,nn)
	return (x) }
########################################################3
px.cn <-function(aPx)
# Cambia atributo dimnames por dimcodes,
# si no exite "dimcodes" lo crea igual a "dimnames"
{
        dn<-dimnames(aPx)
        if (is.null(attr(aPx,"dimcodes"))) attr(aPx,"dimcodes")<-dn
        attr(aPx,"dimnames")<-attr(aPx,"dimcodes")
        attr(aPx,"dimcodes")<-dn
        aPx
        }
########################################################################

"%&%" <- function(c1,c2) { paste(c1,c2,sep="") }

###############################################
lattr<-function (objeto)
# Funcion recursiva que
# lista los atributos de un Objeto
{
  l<-attributes(objeto)
  if (!is.null(l)) {
    print (l)
    l<-names(objeto)
    if (length(l)>1) {
       for (i in l)
         {objeto[i]->p
          print (paste("-------------------",names(p)))
          p[[1]]->p
          lattr(p) } }
  }
}
