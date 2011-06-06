\name{px.agr}
\alias{px.agr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
px.agr(strURL, newDim)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{strURL}{
%%     ~~Describe \code{strURL} here~~
}
  \item{newDim}{
%%     ~~Describe \code{newDim} here~~
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
function (strURL, newDim) 
{
    tf <- tempfile(pattern = "__")
    intentos <- 0
    ee <- 0
    class(ee) <- "try-error"
    while (class(ee) == "try-error" && intentos < 10) {
        intentos <- intentos + 1
        print(paste("INTENTO:", intentos))
        ee <- try(download.file(strURL, tf))
    }
    A <- px.read(tf)$DATA
    A <- mdim.agr(A, newDim, fun = "sum")
    return(A)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
