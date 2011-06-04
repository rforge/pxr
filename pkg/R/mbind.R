mbind<-function(da,...)
# mbind: version multidimensional de rbind y cbind
#   mbind(dA,...)
#   toma un conjunto de arrais multidimensionales ... y las
#   agrega segun la dimension especifica en el 1º elemento
# --- !! falta incluir una comprobación de que las matrices son
#     conformables. son iguales en el resto de las dimensiones
#     no solapables
#
#   Sin una de los parametros esta relleno de NULL lo descarta ...
{
     larg<-list(...)
     while  (is.null(larg[[1]])) {
        larg<-larg[2:length(larg)]
     }
     acumdim<-dim(larg[[1]])
     actudim<-acumdim
     nd<-length(actudim)
     permdim<-1:nd
     permdim[da]<-(1:nd)[nd]
     permdim[nd]<-(1:nd)[da]
     A<-aperm(larg[[1]],permdim)
     A<-c(A)
     dn<-dimnames(larg[[1]])
     if  (length(larg)>=2) {
       for (i in 2:length(larg)) {
         acumdim[da]<-acumdim[da]+dim(larg[[i]])[da]
         A<-c(A,aperm(larg[[i]],permdim))
         dn[[da]]<-c(dn[[da]],dimnames(larg[[i]])[[da]])
        } }
     # nueva array
     acumdimp<-acumdim
     acumdimp[da]<-acumdim[nd]
     acumdimp[nd]<-acumdim[da]
     A<-array(A,acumdimp)
     A<-aperm(A,permdim)
     dimnames(A)<-dn
     return (A)
}
#A<-array(1:24,c(4,3,2))
#dimnames(A)<-list(d1=1:4,d2=1:3,d3=1:2)
#mbind(1,A,A)
