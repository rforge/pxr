\name{piramide}
\alias{piramide}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
piramide(m, main = NULL, prop = TRUE, col = c(5, 6), xlim = NULL, sub = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m}{
%%     ~~Describe \code{m} here~~
}
  \item{main}{
%%     ~~Describe \code{main} here~~
}
  \item{prop}{
%%     ~~Describe \code{prop} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{xlim}{
%%     ~~Describe \code{xlim} here~~
}
  \item{sub}{
%%     ~~Describe \code{sub} here~~
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
function (m, main = NULL, prop = TRUE, col = c(5, 6), xlim = NULL, 
    sub = NULL) 
{
    if (prop) {
        m <- 10^6 * m/sum(m)
        if (is.null(xlim)) 
            xlim = c(-10000, 10000)
        if (is.null(sub)) 
            sub = "Cifras relativas a un mill\303\263n de personas"
    }
    m[, 1] <- -1 * m[, 1]
    barplot(t(m), beside = T, horiz = T, space = c(-1, 0), col = col, 
        xlim = xlim, main = main, sub = sub)
    print(xlim)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
