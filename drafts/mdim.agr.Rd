\name{mdim.agr}
\alias{mdim.agr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mdim.agr(A, newDim, fun = "sum")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{
%%     ~~Describe \code{A} here~~
}
  \item{newDim}{
%%     ~~Describe \code{newDim} here~~
}
  \item{fun}{
%%     ~~Describe \code{fun} here~~
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
function (A, newDim, fun = "sum") 
{
    oldDim <- dimnames(A)
    if (length(oldDim) != length(newDim)) 
        stop("ERROR ..no coinciden las tablas")
    nDim <- length(dim(A))
    dimA <- dimA <- 1:nDim
    for (i in dimA <- (1:length(dim(A)))) {
        sel <- newDim[[i]]
        AA <- apply(A, dimA[-i], function(e) {
            tapply(e, sel, fun, na.rm = T, simplify = TRUE)[unique(sel[!is.na(sel)])]
        })
        cc <- unique(sel[!is.na(sel)])
        if (length(cc) == 1) {
            dn <- dimnames(AA)
            AA <- array(AA, c(1, dim(AA)))
            dimnames(AA) <- c(cc, dn)
            names(dimnames(AA))[1] <- names(oldDim)[i]
        }
        dimP <- order(c(i, dimA[-i]))
        A <- aperm(AA, dimP)
    }
    return(A)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
