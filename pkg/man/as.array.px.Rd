\name{as.array.px}
\alias{as.array.px}
\title{Extraction of data from px objects into arrays }
\description{
This function extracts the data component from a px object as a \code{array}.
}

\usage{
\S3method{as.array}{px}(x, ...)
}
\arguments{
  \item{x}{A \code{px} object.}
  \item{...}{Additional arguments. Not used yet.}
}
\value{An array.
}
\references{
http://www.scb.se/upload/PC-Axis/Support/Documents/PC-Axis_fileformat.pdf
}
\seealso{
  \code{\link{read.px}},
  \code{\link{as.data.frame.px}}
}
\examples{
my.px     <- read.px( system.file( "extdata", "example.px", package = "pxR"))
my.array  <- as.array( my.px )
}
\author{Francisco J. Viciana}
\keyword{manip}

