library(pxR)
library(sp)
library(lattice)
library(latticeExtra)
library(maptools)
#library(classInt)
#library(colorspace)

source('read.ini.R')
source('../pkg/R/as.data.frame.px.R')##nueva versión no incluida en 0.24



unquote <- function(x){
  gsub('\\"', "", x)
}


plot.px <- function(x, select, shpPath = 'shp',  encoding = "latin1", n=10, style='fisher', ...){

  heading <- x$HEADING$value
  vars <- x$VALUES[[heading]]

  datWide <- as.data.frame(x,  use.codes = TRUE, direction = 'wide')  

  mapName <- unique(unquote(x$MAP))[1]
  mapsInfo <- read.pxini(filename = mapName, shpPath=shpPath, encoding=encoding)
  nmaps <- mapsInfo$nummaps

  if (missing(select)) select <- vars

  iCol <- agrep(unique(names(x$MAP))[1], names(datWide))

  fooPlot <- function(i, mapsInfo, datWide, iCol, ...){##n, style, ...) {
    mapi <-  readShapePoly(mapsInfo$filesshp[i])
    field <- mapsInfo$keyfields[i]
    ## Relaciono el data.frame con el shapefile a través de sus IDs

    idx <- match(mapi[[field]], datWide[,iCol])
    dat2add <- data.frame(datWide[idx, select]) ##Hace falta cuando sólo es una columna
    names(dat2add) <- select
    mapi@data <- cbind(mapi@data, dat2add)

    ## int <- classIntervals(c(as.matrix(dat2add)), n, style=style)
    ## cols <- sequential_hcl(length(int$brks)) ##incluirlo como argumento de la función
    p <- spplot(mapi[select], ...)## col.regions=cols, at=int$brks, ...)
    p
  }
  mapsList <- lapply(1:nmaps, fooPlot, mapsInfo, datWide, iCol)##, n, style)
  names(mapsList) <- mapsInfo$keyfields
  do.call(c, mapsList)
}

## Cifras oficiales de población resultantes de la revisión del Padrón municipal a 1 de enero de 2009
## http://www.ine.es/jaxi/menu.do?type=pcaxis&path=%2Ft20%2Fe260%2Fa2009%2F&file=pcaxis&N=&L=0

##ficheros alterados para que usen un mapa más detallado

ficheropx <- read.px(filename="data/pcaxis824627184_2.px")
datPX <- read.px('data/pcaxis-676270323_2.px') 

summary(ficheropx)
summary(datPX)


plot(ficheropx)

plot(datPX)
plot(datPX, select=c('Mujeres', 'Varones'))




