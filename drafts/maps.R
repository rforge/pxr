library(pxR)
library(maptools)
library(classInt)
library(colorspace)

source('read.ini.R')
source('../pkg/R/as.data.frame.px.R')##nueva versión no incluida en 0.24

## setwd('~/Investigacion/pxR/drafts')

unquote <- function(x){
  gsub('\\"', "", x)
}


plotPX <- function(x, select, shpPath = 'shp',  encoding = "latin1", n=10, style='fisher', ...){

  dat <- as.data.frame(x,  use.codes = TRUE)  

  mapName <- unique(unquote(x$MAP))[1]
  mapsInfo <- read.pxini(filename = mapName, shpPath=shpPath, encoding=encoding)
  nmaps <- mapsInfo$nummaps

  heading <- x$HEADING$value
  stub <- x$STUB$value
  vars <- levels(dat[,heading])
  datWide <- reshape(dat, timevar=heading, ## Incorporarlo a as.data.frame.px
                     varying=list(vars),
                     v.names='dat',
                     idvar=stub,
                     direction='wide')

  if (missing(select)) select <- vars

  iCol <- agrep(unique(names(x$MAP))[1], names(datWide))

  fooPlot <- function(i, mapsInfo, datWide, iCol, n, style, ...) {
    mapi <-  readShapePoly(mapsInfo$filesshp[i])
    field <- mapsInfo$keyfields[i]
    ## Relaciono el data.frame con el shapefile a través de sus IDs

    idx <- match(mapi[[field]], datWide[,iCol])
    dat2add <- data.frame(datWide[idx, select]) ##Hace falta cuando sólo es una columna
    names(dat2add) <- select
    mapi@data <- cbind(mapi@data, dat2add)

    int <- classIntervals(c(as.matrix(dat2add)), n, style=style)
    cols <- sequential_hcl(length(int$brks)) ##incluirlo como argumento de la función
    p <- spplot(mapi[select], col.regions=cols, at=int$brks, ...)
    p
  }
  mapsList <- lapply(1:nmaps, fooPlot, mapsInfo, datWide, iCol, n, style, ...)
  names(mapsList) <- mapsInfo$keyfields
  do.call(c, mapsList)
}


ficheropx <- read.px(filename="data/pcaxis824627184.px")
## Cifras oficiales de población resultantes de la revisión del Padrón municipal a 1 de enero de 2009
## http://www.ine.es/jaxi/menu.do?type=pcaxis&path=%2Ft20%2Fe260%2Fa2009%2F&file=pcaxis&N=&L=0
datPX <- read.px('data/pcaxis-676270323.px')

summary(ficheropx)
summary(datPX)


plotPX(ficheropx)

plotPX(datPX)
plotPX(datPX, select=c('Mujeres', 'Varones'))




##problema con Madrid y Andalucía
codes <- as.data.frame(ficheropx, use.codes=TRUE)[,2]
nms <- as.data.frame(ficheropx)[,2]
data.frame(codes=codes, names=nms)
##Madrid no está como provincia, sólo como Comunidad
##...pero Andalucía sí está como Comunidad ¿¿??

