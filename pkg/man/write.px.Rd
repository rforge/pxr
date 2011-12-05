\name{write.px}
\alias{write.px}
\title{
Write a PC-Axis file
}
\description{
  
  This function write a object of class \code{px} to a PC-Axis file 

}
\usage{
write.px(obj.px ,filename, encod="ISO_8859-1")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj.px}{ The name of a objest of class 'px' }
  \item{filename}{ The name of the PC-Axis file to create }
  \item{encod}{A character string describing the encoding tu use in
    px file
    (see \code{\link{iconv}} for details).}
}
% \details{
% TBA
% }
\value{
  An file that can be used with  \code{PC-AXIS} program (see references for details).
}
\references{
\url{http://www.scb.se/upload/PC-Axis/Support/Documents/PC-Axis_fileformat.pdf}
}
\author{
Francisco J. Viciana Fernández, Oscar Perpiñan Lamigueiro, Carlos J. Gil Bellosta
}
% \note{
%  ~~further notes~~
% }
\seealso{
  \code{\link{read.px}},  
  \code{\link{as.data.frame.px}},
  \code{\link{as.array.px}}
  \code{\link{iconv}}
}
\examples{


opx1 <- read.px(  system.file( "extdata", "example.px", package = "pxR")  )  
write.px ( opx1, file = 'opx.px')  #  write a   copy
opx2 <- read.px  ('opx.px')        #  read  the copy

### are de same data ?
as.array(opx1)-> a1
as.array(opx2)-> a2
sum(a1-a2)


}

\keyword{ manip }
