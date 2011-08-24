\name{read.px}
\alias{read.px}
\title{
Reads a PC-Axis file
}
\description{
This function reads a PC-AXIS file (a text file with certain format) and creates an object of the class px.
}
\usage{
read.px( filename, encod.from = "ISO_8859-1", encod.to = "UTF-8", na.string='"."')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{ The name of the PC-Axis file to read }
  \item{encod.from}{A character string describing the current encoding
    (see \code{\link{iconv}} for details).}
  \item{encod.to}{A character string describing the target encoding (see
    \code{\link{iconv}} for details).}
  \item{na.string}{A character to be interpreted as missing value in
    the \code{DATA} field of the PC-AXIS file. The chapter 4 of the
    second reference provides details about the dot codes used in the
    PC-AXIS format.}
}
% \details{
% TBA
% }
\value{
An object of the class \code{px}, which is essentially a list comprising the
fields of a PC-AXIS file (see references for details).
}
\references{
  \url{http://www.scb.se/upload/PC-Axis/Support/Documents/PC-Axis_fileformat.pdf}
  \url{http://tilastokeskus.fi/tup/pcaxis/tiedostomuoto2006_laaja_en.pdf}
}
\author{
Carlos J. Gil Bellosta, Oscar Perpiñan Lamigueiro, Francisco J. Viciana Fernández
}
% \note{
%  ~~further notes~~
% }
\seealso{
  \code{\link{as.data.frame.px}},
  \code{\link{as.array.px}}
  \code{\link{iconv}}
}
\examples{

my.px.object <- read.px(system.file("extdata", "example.px",
                        package = "pxR"), encod.from='UTF-8')
my.px.data   <-  as.data.frame( my.px.object ) 

}

\keyword{ manip }
