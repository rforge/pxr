\name{px.read}
\alias{px.read}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
px.read(file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
%%     ~~Describe \code{NameFile} here~~
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
function (NameFile) 
{
    px <- list()
    zz <- NULL
    px.c <- NULL
    lp <- ""
    nl <- 0
    NamesVar = c()
    a <- NULL
    zz <- file(NameFile, "rt")
    px.c <- c()
    px.dat <- c()
    is.ansi <- FALSE
    while (strsplit(l <- readLines(zz, 1), "=")[[1]][1] != "DATA") {
        l <- gsub(";[[:space:]]+$", ";", l)
        nl <- nl + 1
        if (substr(l, nchar(l), nchar(l)) == ";") {
            lp <- paste(lp, substr(l, 1, nchar(l) - 1), sep = "")
            px.c <- c(px.c, lp)
            lp <- ""
        }
        else lp <- paste(lp, l, sep = "")
    }
    px.c <- gsub("^[[:space:]]+", "", px.c)
    px.c <- gsub("[[:space:]]+$", "", px.c)
    for (i in px.c) {
        cab <- strsplit(i, "=")[[1]][1]
        col <- strsplit(i, "=")[[1]][2]
        if (cab == "ANSI") {
            is.ansi <- TRUE
        }
        if (cab == "STUB") {
            NamesVar <- c(NamesVar, rev(gsub("\"", "", strsplit(col, 
                "\", +|\",")[[1]])))
        }
        else if (cab == "HEADING") {
            NamesVar <- c(rev(gsub("\"", "", strsplit(col, "\", +|\",")[[1]])), 
                NamesVar)
        }
        lw <- c("SUBJECT-AREA", "SUBJECT-CODE", "MATRIX", "TITLE", 
            "CONTENTS", "UNITS")
        if (length(grep(paste("^", cab, sep = ""), lw)) > 0) {
            px[[cab]] = gsub("\"", "", col)
        }
    }
    NamesVar <- oem2ansi(NamesVar)
    for (i in 1:length(px)) {
        if (!is.ansi) {
            px[[i]] <- oem2ansi(px[[i]])
        }
    }
    for (i in 1:length(px.c)) if (!is.ansi) {
        px.c[[i]] <- oem2ansi(px.c[[i]])
    }
    listcat = list()
    listcod = list()
    for (i in NamesVar) {
        listcat[[i]] <- c(NA)
        listcod[[i]] <- c(NA)
    }
    for (i in px.c) {
        cab <- strsplit(i, "=")[[1]][1]
        col <- strsplit(i, "=")[[1]][2]
        if (substr(cab, 1, 6) == "VALUES") {
            var <- gsub("\"", "", strsplit(cab, "\\(\"|\"\\)")[[1]][2])
            val <- gsub("\"", "", strsplit(col, "\", +|\",")[[1]])
            listcat[[var]] <- val
        }
        if (substr(cab, 1, 5) == "CODES") {
            var <- gsub("\"", "", strsplit(cab, "\\(\"|\"\\)")[[1]][2])
            val <- gsub("\"", "", strsplit(col, "\", +|\",")[[1]])
            listcod[[var]] <- val
        }
    }
    px.dat <- readLines(zz, -1, ok = TRUE)
    l <- gsub("DATA=", "", l)
    px.dat <- paste(l, px.dat, sep = " ")
    px.dat <- gsub(";", "", px.dat)
    px.dat <- px.dat[px.dat != ""]
    px.dat <- paste(px.dat, collapse = " ")
    px.dat <- gsub("\"..\"", "NA", px.dat)
    px.dat <- gsub("\".\"", "NA", px.dat)
    px.dat <- gsub("[[:space:]]+", " ", px.dat)
    px.dat <- gsub("^[[:space:]]+", "", px.dat)
    px.dat <- gsub("[[:space:]]+$", "", px.dat)
    px.dat <- as.numeric(strsplit(px.dat, " ")[[1]])
    a <- array(px.dat, unlist(lapply(listcat, length)), dimnames = listcat)
    a <- aperm(a, length(dim(a)):1)
    px$DATA <- a
    px$HEADING <- NamesVar[1]
    px$STUB <- NamesVar[-1]
    dd <- rev(1:length(dim(px$DATA)))
    dd[1:2] <- dd[c(2:1)]
    px$DATA <- aperm(px$DATA, dd)
    if (!is.null(listcod)) {
        dc <- dimnames(px$DATA)
        for (i in names(dc)) dc[[i]] <- listcod[[i]]
        attr(px$DATA, "dimcodes") <- dc
    }
    close(zz, "rt")
    px
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
