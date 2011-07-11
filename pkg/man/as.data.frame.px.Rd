\name{as.data.frame.px}
\alias{as.data.frame.px}
\alias{as.data.frame}

\title{
Extraction of data from px into data.frame objects
}
\description{
This function extracts the data component from a px object as a \code{data.frame}.
}
\usage{
\S3method{as.data.frame}{px}( x, row.names, optional, ... )
}
\arguments{
  \item{x}{ a px object }
  \item{row.names}{ Not used; ignored }
  \item{optional}{ Not used; ignored }
  \item{...}{ Additional arguments, currently not used }
}
\details{
TBA
}
\value{
A data.frame object.
}
\references{
http://www.scb.se/upload/PC-Axis/Support/Documents/PC-Axis_fileformat.pdf
}
\author{
Carlos J. Gil Bellosta, Oscar Perpiñán Lamigueiro, Francisco J. Viciana Fernández
}
%\note{ }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{read.px}},
  \code{\link{as.array.px}}
  }

\examples{
my.px.object <- read.px( system.file( "extdata", "example.px", package = "pxR") )
my.px.data   <- as.data.frame( my.px.object )
}
\keyword{ manip }
