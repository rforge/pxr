\name{as.px}
\alias{as.px}
\alias{as.px.array}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  %%  ~~function to do ... ~~
  Convert an array into an object of the class px
}

\description{
  %% ~~ A concise (1-5 lines) description of what the function does. ~~
  This function convert a array object to a \code{px} object. Use the
  code{dimnames} of the array to fill the \code{VALUES} list of
  \code{px} object. Optionally it uses an object of type px as the
  skeleton of the new object } 

\usage{
as.px( x, ... )
\S3method{as.px}{array}( x, skeleton.px = NULL, list.keys = NULL, ...  )
}

\arguments{
  \item{x}{ A array with the data entered in the \code{px} object}
  \item{skeleton.px}{ Properties not included in the array will be taken of this \code{px} object }
  \item{list.keys}{ A optional list with pair of keys and values to append and modify \code{px} object}
  \item{...}{ Other arguments that can be passed to the function }

}
\details{
  %%  ~~ If necessary, more details than the description above ~~
  This function is used to transfer information from an array
  R objects to a PX objects.
}
\value{
%%  ~Describe the value returned
a \code{px} object
}

\author{Francisco J. Viciana, Carlos J. Gil Bellosta,}
%%  ~~who you are~~

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  %% ~~objects to See Also as \code{\link{help}}, ~~~
  \code{\link{write.px}},
  \code{\link{as.array.px}}
}
\examples{
my.px.object  <- read.px( system.file( "extdata", "example.px", package = "pxR") )
my.data       <- as.array( my.px.object )
my.px.object2 <- as.px( my.data )
my.px.object3 <- as.px( my.data, skeleton.px = my.px.object ) 

### export data checks
sum(my.data - as.array( my.px.object2) )
sum(my.data - as.array( my.px.object3) )

### Checks writing for missing data
oo  <- read.px(  system.file( "extdata", "example2.px", package = "pxR"))
aa  <-  as.array(oo)
aa[ sample( 1:length(aa), 5 ) ] <- NA
write.px( as.px(aa), file = 'tmp01.px' )

### append and modify keys
write.px(as.px.array(aa,skeleton.px=oo),file='tmp02.px')
write.px(as.px.array(aa,
             list.keys= list(MATRIX='xxx', CONTENTS='new data',
                             NEWKEY='an other key',
                             UNITS='people', TITLE='My Title') 
                     ),file='tmp03.px')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }

