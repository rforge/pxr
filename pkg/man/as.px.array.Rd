\name{as.px.array}
\alias{as.px}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  %%  ~~function to do ... ~~
  Convert a array to a px object
}
\description{
  %% ~~ A concise (1-5 lines) description of what the function does. ~~
  This function convert a array object to a \code{px} object. Use the
  code{dimnames} of the array to fill the \code{VALUES} list of
  \code{px} object. Optionally it uses an object of type px as the
  skeleton of the new object } \usage{ as.px.array(obj.array,
  skeleton.px = NULL) }
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj.array}{
    %%     ~~Describe \code{obj.array} here~~
    A array with the data entered in the \code{px} object
}
\item{skeleton.px}{
   Properties not included in the array will be taken of this \code{px} object
%%     ~~Describe \code{skeleton.px} here~~
}
}
\details{
  %%  ~~ If necessary, more details than the description above ~~
  This function is used to transfer information from the usual
  R objects to PX objects and files 
}
\value{
%%  ~Describe the value returned
a \code{px} object
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Francisco Viciana}
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  %% ~~objects to See Also as \code{\link{help}}, ~~~
  \code{\link{write.px}},
  \code{\link{as.array.px}}
}
\examples{
oo  <- read.px(  system.file( "extdata", "example.px", package = "pxR"))
aa  <- as.array(oo)
as.px.array(aa)->px.aa1
as.px.array(aa,skeleton.px=oo )->px.aa2

as.array(px.aa1)->aa1
str(aa) ; str (aa1)

sum(as.array(aa)-as.array(px.aa1))
sum(as.array(aa)-as.array(px.aa2))

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }

