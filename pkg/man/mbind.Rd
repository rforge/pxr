\name{mbind}
\alias{mbind}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mbind(da, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{da}{
%%     ~~Describe \code{da} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (da, ...) 
{
    larg <- list(...)
    while (is.null(larg[[1]])) {
        larg <- larg[2:length(larg)]
    }
    acumdim <- dim(larg[[1]])
    actudim <- acumdim
    nd <- length(actudim)
    permdim <- 1:nd
    permdim[da] <- (1:nd)[nd]
    permdim[nd] <- (1:nd)[da]
    A <- aperm(larg[[1]], permdim)
    A <- c(A)
    dn <- dimnames(larg[[1]])
    if (length(larg) >= 2) {
        for (i in 2:length(larg)) {
            acumdim[da] <- acumdim[da] + dim(larg[[i]])[da]
            A <- c(A, aperm(larg[[i]], permdim))
            dn[[da]] <- c(dn[[da]], dimnames(larg[[i]])[[da]])
        }
    }
    acumdimp <- acumdim
    acumdimp[da] <- acumdim[nd]
    acumdimp[nd] <- acumdim[da]
    A <- array(A, acumdimp)
    A <- aperm(A, permdim)
    dimnames(A) <- dn
    return(A)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
