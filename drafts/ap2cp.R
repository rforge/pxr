ap2cp <-function(AP)
# Imput: a matrix(axp) with dimnames:
#    ages:   in row
#    period: in column
# Output: matrix(a,p+a-1)
#    cohort: in row
#    period: in column
{
	# Make matrix receptora
        coh <- array(NA,c(sum(dim(AP))-1,dim(AP)[2]))
        # Rango PeriodosPeriodo inicial y final
        rp <- range(as.numeric(dimnames(AP)[[2]]))
        # Rango Edades Inicial y final
        ra <- range(as.numeric(dimnames(AP)[[1]]))
        rc <- c((rp[1]-ra[2]-1),(rp[2]-ra[1]-1))
	dimnames(coh) <- list(as.character((rp[1]-ra[2]-1):(rp[2]-ra[1]-1)),
                              as.character(rp[1]:rp[2]))
	# - Rellena Matriz de cohortes
	for (a in ra[1]:ra[2]) {
	     for (p in rp[1]:rp[2]) {
	     	 coh[as.character(p-a-1),
                     as.character(p)]<-AP[as.character(a),
	     	                               as.character(p)]
	     	}
	}
  return (coh)
  }
