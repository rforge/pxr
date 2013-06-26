\name{as.px}
\alias{as.px}
\alias{as.px.array}


\title{ Convert an array into an object of the class px }

\description{
  This function converts an array object into a \code{px} object. It uses the
  code{dimnames} of the array to fill the \code{VALUES} list of the
  \code{px} object. Optionally it uses an object of type \code{px} as the
  skeleton of the new object. } 

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
  This function is used to transfer information from an array
  R objects to a PX objects.
}
\value{ a \code{px} object }

\author{Francisco J. Viciana, Carlos J. Gil Bellosta,}


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
aa  <- as.array(oo)
aa[ sample( 1:length(aa), 5 ) ] <- NA
write.px( as.px(aa), file = 'tmp01.px' )

### append and modify keys
write.px( as.px.array(aa,skeleton.px=oo), file='tmp02.px' )
write.px( as.px.array( aa,
             list.keys= list(MATRIX='xxx', CONTENTS='new data',
                             NEWKEY='an other key',
                             UNITS='people', TITLE='My Title') 
                     ), file='tmp02.px')

### collapses a dimension
oo  <- read.px( system.file( "extdata", "example2.px", package = "pxR"))
aa  <- as.array(oo) 
aa  <- apply( aa, 1:2, sum ) 
oo2 <- as.px.array( aa, skeleton.px = oo ) 
write.px (oo2, file='tmp03.px')

### remove temporal files
file.remove('tmp04.px','tmp03.px','tmp02.px','tmp01.px')

}

\keyword{ manip }

