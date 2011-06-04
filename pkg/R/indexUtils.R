index<-function(j,dimension)
# index(j,dimesion)
# Dado una posición absoluta "j" en un vector calcula la posición que le
# corresponderia en una matriz de dimensiones "dimension"

{
 .index <- function(j,dimension) {
   indice<-1+(j-1)%%dimension[1]
   for (i in 2:length(dimension)) {
        indice<-c(indice,
              1+((floor((j-1)/prod(dimension[1:(i-1)]))+1)-1)%%dimension[i] )}
   return(indice)
 }
 j<-c(j)
 indice<-.index(j[1],dimension)
 if (length(j)>1) {
        indice<-matrix(indice,nrow=1)
        for (i in j[-1])
          indice<-rbind(indice,matrix(.index(j[i],dimension),nrow=1))
      }
 return(indice)
}
# A<-array(1:24,c(4,3,2))
# dimnames(A)<-list(d1=1:4,d2=1:3,d3=1:2)
# index(2,c(4,3,2))
# index(c(A),c(4,3,2))
############################################################################
i2i<-function(i,A)
# transforma una tabla de indices tipo caracter
# en sus correspondientes indice numericos,
# para extración de valores de una array multidimensional
# i: tabla de indices
#      (matriz: n.filas es el número de elementos a extraer
#       columnas: n. de dimensiones de A
# A: array multimensiona de la que se desea la extración
#    en base al dimnames(A)
{
    nA<-dimnames(A)
    ii<-NULL
    for (e in 1:length(nA)) {
        ii<-cbind(ii,match(i[,e],nA[[e]]))
        }
    ii
}
# A<-array(1:24,c(4,3,2))
# i<-array(as.character(c(1,4,1,3,1,2)),c(2,3))
# dimnames(A)<-list(d1=1:4,d2=1:3,d3=1:2)
# i2i(i,A)
