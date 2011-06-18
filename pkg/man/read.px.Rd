\name{read.px}
\alias{read.px}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reads a PC-Axis file
}
\description{
This function reads a PC-AXIS file (a text file with certain format) and creates an object of the class px.
}
\usage{
read.px( filename )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{ The name of the PC-Axis file to read }
}
\details{
TBA
}
\value{
An object of the class px, which is essentially...
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

}
\examples{

my.px.object <- read.px( system.file( "extdata", "example.px", package = "pxR") )
my.px.data   <-  as.data.frame( my.px.object ) 

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
