\name{summary.px}
\alias{summary.px}
\alias{summary}

\title{
Extraction of data from px object into a data.frame
}
\description{
This function extracts the data component from a px object as a \code{data.frame}.
}
\usage{
\method{summary}{px}( object, ... )
}
\arguments{
  \item{object}{ a px object }
  \item{...}{ Additional arguments, currently not used }
}
\details{
TBA
}
\value{
A verbose description of data and metadata within the px object.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Carlos J. Gil Bellosta
}
\note{ }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ }
\examples{
my.px.object <- read.px( system.file( "extdata", "example.px", package = "pxR") )
summary( my.px.object )
}
\keyword{ manip }
