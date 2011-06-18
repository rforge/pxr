\name{sa2aa}
\alias{sa2aa}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sa2aa(PFinEdad)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{PFinEdad}{
%%     ~~Describe \code{PFinEdad} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Francisco J. Viciana Fernández
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(PFinEdad)
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
  PFinGEdad <- apply(PFinEdad, c(2,3,4), function(x)  return(x\%*\%mGEdad))
  names(dimnames(PFinGEdad))[1] <- "GEdad"
  dimnames(PFinGEdad)[[1]] <- GEdadnombres
  return(PFinGEdad)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
