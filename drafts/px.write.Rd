\name{px.write}
\alias{px.write}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
px.write(data, file = "", title = "Sin titulo", contens = "tipo contenido", units = "cosas", subjectarea = "00", subjectcode = "xx", showdecimals = 5, decimals = 2, format = "f", ANSI = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{file}{
%%     ~~Describe \code{file} here~~
}
  \item{title}{
%%     ~~Describe \code{title} here~~
}
  \item{contens}{
%%     ~~Describe \code{contens} here~~
}
  \item{units}{
%%     ~~Describe \code{units} here~~
}
  \item{subjectarea}{
%%     ~~Describe \code{subjectarea} here~~
}
  \item{subjectcode}{
%%     ~~Describe \code{subjectcode} here~~
}
  \item{showdecimals}{
%%     ~~Describe \code{showdecimals} here~~
}
  \item{decimals}{
%%     ~~Describe \code{decimals} here~~
}
  \item{format}{
%%     ~~Describe \code{format} here~~
}
  \item{ANSI}{
%%     ~~Describe \code{ANSI} here~~
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
function (data, file = "", title = "Sin titulo", contens = "tipo contenido", 
    units = "cosas", subjectarea = "00", subjectcode = "xx", 
    showdecimals = 5, decimals = 2, format = "f", ANSI = TRUE) 
{
    dc <- attr(data, "dimcodes")
    dd <- 1:length(dim(data))
    dd[1:2] <- dd[c(2:1)]
    data <- aperm(data, dd)
    cat(paste("CHARSET=\"", ifelse(ANSI, "ANSI", "OEM"), "\";\n", 
        sep = ""), file = file)
    cat("AXIS-VERSION=\"2000\";\n", file = file, append = T, 
        sep = "")
    cat("SUBJECT-AREA=\"", subjectarea, "\";\n", file = file, 
        append = T, sep = "")
    cat("SUBJECT-CODE=\"", subjectcode, "\";\n", file = file, 
        append = T, sep = "")
    cat(paste("MATRIX=\"", strsplit(basename(file), "\\.")[[1]][1], 
        "\";\n", sep = ""), file = file, append = T)
    cat(paste("TITLE=\"", title, "\"\n\"por ", paste(names(dimnames(data)), 
        collapse = ", "), "\";\n", sep = ""), file = file, append = T)
    cat(paste("CONTENTS=\"", contens, "\";\n", sep = ""), file = file, 
        append = T)
    cat(paste("STUB=\"", paste(rev(names(dimnames(data))[-1]), 
        collapse = "\",\""), "\";\n", sep = ""), file = file, 
        append = T)
    cat(paste("HEADING=\"", names(dimnames(data))[1], "\";\n", 
        sep = ""), file = file, append = T)
    for (n in (length(dim(data))):1) {
        cat(paste("VALUES", "(\"", names(dimnames(data))[n], 
            "\")=\"", paste(dimnames(data)[[n]], collapse = "\",\"", 
                sep = ""), "\";\n", sep = ""), file = file, append = T)
    }
    cat(paste("UNITS=\"", units, "\";\n", sep = ""), file = file, 
        append = T)
    if (!is.null(dc)) {
        for (n in (length(dim(data))):1) {
            cat(paste("CODES", "(\"", names(dc)[n], "\")=\"", 
                paste(dc[[n]], collapse = "\",\"", sep = ""), 
                "\";\n", sep = ""), file = file, append = T)
        }
    }
    cat(paste("DECIMALS=", decimals, ";\n", sep = ""), file = file, 
        append = T)
    cat(paste("SHOWDECIMALS=", showdecimals, ";\n", sep = ""), 
        file = file, append = T)
    cat("DATA=\n", file = file, append = T)
    data <- round(data, digits = decimals)
    write(data, file = file, ncolumns = dim(data)[1], append = T)
    cat(";", file = file, append = T)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
