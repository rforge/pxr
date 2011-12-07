library(pxR)
library(maptools)
library(classInt)
library(colorspace)

unquote <- function(x){
  gsub('\\"', "", x)
}

##Ejemplo de Oscar

## setwd('~/Investigacion/pxR/drafts')

## Cifras oficiales de población resultantes de la revisión del Padrón municipal a 1 de enero de 2009
## http://www.ine.es/jaxi/menu.do?type=pcaxis&path=%2Ft20%2Fe260%2Fa2009%2F&file=pcaxis&N=&L=0

datPX <- read.px('data/pcaxis-676270323.px')
dat <- as.data.frame(datPX,  use.codes = TRUE)

##Me quedo sólo con Total (dejo fuera distinción por sexo)
datTotal <- dat[dat$sexo == 'Total',- 1]
##elimino fila de Total
datTotal <- datTotal[-1,]

## ShapeFiles de
## http://www.ine.es/ss/Satellite?blobcol=urldata&blobheader=application%2Fzip&blobheadername1=Content-Disposition&blobheadervalue1=attachment%3B+filename%3Dmapas_completo_municipal.zip&blobkey=urldata&blobtable=MungoBlobs&blobwhere=250%2F237%2Fmapas_completo_municipal%2C2.zip&ssbinary=true
nombremapa <- unquote(datPX$MAP[1]) ##ups, lo ponen dos veces,  de ahí el [1]
mapas <- read.pxini(nombremapa)$filesshp
mapa <- readShapePoly(mapas)
mapa$NOMBRE99
mapa$PROV
dim(mapa)
## Relaciono el data.frame con el shapefile a través de sus IDs
## Debieramos probar a hacer esto con spCbind
idx <- match(mapa$PROV,  datTotal$provincias)
mapa$dat <- datTotal$dat[idx]

##Elaboro mapa. Elijo intervalos con classIntervals
int <- classIntervals(mapa$dat, 10, style='jenks')
cols <- heat_hcl(10)
spplot(mapa['dat'], col.regions=cols, at=int$brks)

####################################################################################

## Ejemplo de Emilio
ficheropx <- read.px(filename="data/pcaxis824627184.px")
summary(ficheropx)

plotPX <- function(x){
  dat <- as.data.frame(x,  use.codes = TRUE)  
  mapName <- unquote(x$MAP)
  mapsInfo <- read.pxini(filename = mapName)
  nmaps <- mapsInfo$nummaps
  iCol <- agrep(names(x$MAP), names(dat))
  mapsList <- lapply(1:nmaps, function(i, mapsInfo, dat, iCol) {
     mapi <-  readShapePoly(mapsInfo$filesshp[i])
     field <- mapsInfo$keyfields[i]
     ## Relaciono el data.frame con el shapefile a través de sus IDs
     ## Debieramos probar a hacer esto con spCbind
     idx <- match(mapi[[field]], dat[,iCol])
     mapi$dat <- dat$dat[idx]
     int <- classIntervals(mapi$dat, 10, style='jenks')
     cols <- heat_hcl(10)
     p <- spplot(mapi['dat'], col.regions=cols, at=int$brks)
     p
   }, mapsInfo, dat, iCol)
  names(mapsList) <- mapsInfo$keyfields
  do.call(c, mapsList)
}

plotPX(ficheropx)


##problema con Madrid y Andalucía
codes <- as.data.frame(ficheropx, use.codes=TRUE)[,2]
nms <- as.data.frame(ficheropx)[,2]
data.frame(codes=codes, names=nms)
##Madrid no está como provincia, sólo como Comunidad
##...pero Andalucía sí está como Comunidad ¿¿??
