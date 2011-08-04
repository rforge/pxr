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
\S3method{as.data.frame}{px}( x, row.names, optional, use.codes = FALSE, ... )
}
\arguments{
  \item{x}{ a px object }
  \item{row.names}{ Not used; ignored }
  \item{optional}{ Not used; ignored }
  \item{use.codes}{ If true, this parameter replaces the names of the variables by their codes, if available. See details section. }
  \item{...}{ Additional arguments, currently not used }
}
\details{
    The PC-Axis provides two alternative mechanisms for naming variables: their values or their codes, usually less verbose.
    Users of the package may use the \code{use.codes} parameter to use the CODES attribute in the PC-Axis file for naming the levels of the variables.
    It should be noted that there may not be CODES for all variables; in such cases, the VALUES are used instead.
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
