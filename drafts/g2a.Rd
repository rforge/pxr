\name{g2a}
\alias{g2a}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
g2a(PFin)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{PFin}{
%%     ~~Describe \code{PFin} here~~
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
function(PFin)
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
