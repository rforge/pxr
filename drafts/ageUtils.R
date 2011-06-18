g2a <- function(PFin)
# generación a edad
# 1º dimension generacion anual
# 3º dimensión año calendario
{
   PFinEdad <- PFin;
   PFinEdad[] <- 0
   names(dimnames(PFinEdad))[1] <- "Edad"
   dimnames(PFinEdad)[[1]] <- (dim(PFin)[1]:1)-1
   for (j in 1:dim(PFinEdad)[3]){
	PFinEdad[(1-j+dim(PFinEdad)[3]):dim(PFinEdad)[1],,j,] <-
	PFin[1:(dim(PFinEdad)[1]+j-dim(PFinEdad)[3]),,j,]
   }
   PFinEdad <- PFinEdad[order(as.numeric(dimnames(PFinEdad)[[1]])),,,];
   return(PFinEdad)
}
sa2aa<-function(PFinEdad)
# 1º dimension edad simple
# obligatorio 4 dimensiones
# salida grupos de quinquenales 0,1-4,5-9,...85y+
{
  mGEdad <- matrix(c(c(1,rep(0,dim(PFinEdad)[1] -1)), c(0,rep(1,4),rep(0,dim(PFinEdad)[1]-5))),nrow=dim(PFinEdad)[1],ncol=2)
  GEdadnombres <- c("0 años", "De 1 a 4 años")
  for (j in 3:18){
	mGEdad <- cbind(mGEdad, c(rep(0,5*(j-2)),rep(1,5),rep(0,dim(PFinEdad)[1]+5-5*j)))
	GEdadnombres <- c(GEdadnombres, paste("De",5*(j-2),"a",5*(j-2)+4,"años",sep=" "))
  }
  mGEdad <- cbind(mGEdad, c(rep(0,85),rep(1,dim(PFinEdad)[1]-85)))
  GEdadnombres <- c(GEdadnombres, "85 y más años")
  PFinGEdad <- apply(PFinEdad, c(2,3,4), function(x)  return(x%*%mGEdad))
  names(dimnames(PFinGEdad))[1] <- "GEdad"
  dimnames(PFinGEdad)[[1]] <- GEdadnombres
  return(PFinGEdad)
}

