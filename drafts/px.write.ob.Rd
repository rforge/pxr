\name{px.write.ob}
\alias{px.write.ob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
px.write.ob(data, file = "", ANSI = TRUE, decimals = 2, format = "f")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{file}{
%%     ~~Describe \code{file} here~~
}
  \item{ANSI}{
%%     ~~Describe \code{ANSI} here~~
}
  \item{decimals}{
%%     ~~Describe \code{decimals} here~~
}
  \item{format}{
%%     ~~Describe \code{format} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (data, file = "", ANSI = TRUE, decimals = 2, format = "f") 
{
    px.write(data, file = file, title = data$TITLE, contens = data$CONTENTS, 
        units = data$UNITS, subjectarea = data$SUBJECT - AREA, 
        subjectcode = data$SUBJECT - CODE, decimals = decimals, 
        format = format, ANSI = ANSI)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
