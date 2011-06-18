\name{fv.matplot}
\alias{fv.matplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fv.matplot(x, mDat, type = "l", legend = NULL, xylegend = NULL, ljust = c(1, 1), lcex = 0.7, lwd = c(2, 1, 1, 2, 1), lty = c(1, 2, 3, 1, 5), col = c(1, 4, 3, 2, 5), ylab = NULL, xlab = NULL, bg = NULL, main = "", ylim = NULL, xlim = NULL, add = F, cex.axis = NULL, cex.main = NULL, cex.lab = NULL, pch = NULL, atx = NULL, aty = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{mDat}{
%%     ~~Describe \code{mDat} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{legend}{
%%     ~~Describe \code{legend} here~~
}
  \item{xylegend}{
%%     ~~Describe \code{xylegend} here~~
}
  \item{ljust}{
%%     ~~Describe \code{ljust} here~~
}
  \item{lcex}{
%%     ~~Describe \code{lcex} here~~
}
  \item{lwd}{
%%     ~~Describe \code{lwd} here~~
}
  \item{lty}{
%%     ~~Describe \code{lty} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
}
  \item{bg}{
%%     ~~Describe \code{bg} here~~
}
  \item{main}{
%%     ~~Describe \code{main} here~~
}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
}
  \item{xlim}{
%%     ~~Describe \code{xlim} here~~
}
  \item{add}{
%%     ~~Describe \code{add} here~~
}
  \item{cex.axis}{
%%     ~~Describe \code{cex.axis} here~~
}
  \item{cex.main}{
%%     ~~Describe \code{cex.main} here~~
}
  \item{cex.lab}{
%%     ~~Describe \code{cex.lab} here~~
}
  \item{pch}{
%%     ~~Describe \code{pch} here~~
}
  \item{atx}{
%%     ~~Describe \code{atx} here~~
}
  \item{aty}{
%%     ~~Describe \code{aty} here~~
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
function (x, mDat, type = "l", legend = NULL, xylegend = NULL, 
    ljust = c(1, 1), lcex = 0.7, lwd = c(2, 1, 1, 2, 1), lty = c(1, 
        2, 3, 1, 5), col = c(1, 4, 3, 2, 5), ylab = NULL, xlab = NULL, 
    bg = NULL, main = "", ylim = NULL, xlim = NULL, add = F, 
    cex.axis = NULL, cex.main = NULL, cex.lab = NULL, pch = NULL, 
    atx = NULL, aty = NULL, ...) 
{
    matplot(x, mDat, type = type, lwd = lwd, lty = lty, col = col, 
        xlab = xlab, ylab = ylab, axes = F, cex.lab = cex.lab, 
        ylim = ylim, xlim = xlim, add = add, pch = pch)
    if (is.null(xylegend)) {
        xylegend <- c(min(x), max(mDat, na.rm = T))
    }
    if (is.null(atx)) {
        if (is.null(xlim)) {
            atx = pretty(x)
        }
        else atx = pretty(xlim)
    }
    axis(1, at = atx, line = 1, cex.axis = cex.axis)
    if (is.null(aty)) {
        if (is.null(ylim)) {
            aty = pretty(mDat[!is.na(mDat)])
        }
        else aty = pretty(ylim)
    }
    axis(2, at = aty, line = 0.5, cex.axis = cex.axis, cex.lab = cex.lab)
    if (!is.null(legend)) {
        legend(xylegend[1], xylegend[2], legend = legend, lwd = lwd, 
            lty = lty, col = col, cex = lcex, yjust = ljust[2], 
            xjust = ljust[1])
    }
    title(main = main, cex.main = cex.main)
    grid(col = "gray50")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
