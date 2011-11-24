library(pxR)
library(maptools)

##Ejemplo de Oscar

## setwd('~/Investigacion/pxR/drafts')

## Cifras oficiales de población resultantes de la revisión del Padrón municipal a 1 de enero de 2009
## http://www.ine.es/jaxi/menu.do?type=pcaxis&path=%2Ft20%2Fe260%2Fa2009%2F&file=pcaxis&N=&L=0

datPX <- read.px('pcaxis-676270323.px')
dat <- as.data.frame(datPX)
##Me quedo sólo con Total (dejo fuera distinción por sexo)
datTotal <- dat[dat$sexo == 'Total',- 1]
##Extraigo el ID de cada provincia, necesario para relacionarlo con el Shapefile
provID <- strsplit(as.character(datTotal$provincias), split = ' ')
provID <- sapply(provID,  function(x)x[1])
datTotal$ID <- provID
##elimino fila de Total
datTotal <- datTotal[-1,]

## ShapeFiles de
## http://www.ine.es/ss/Satellite?blobcol=urldata&blobheader=application%2Fzip&blobheadername1=Content-Disposition&blobheadervalue1=attachment%3B+filename%3Dmapas_completo_municipal.zip&blobkey=urldata&blobtable=MungoBlobs&blobwhere=250%2F237%2Fmapas_completo_municipal%2C2.zip&ssbinary=true

mapa <- readShapePoly('spain_provinces_img_ind_2.shp')
mapa$NOMBRE99
mapa$PROV
dim(mapa)
## Relaciono el data.frame con el shapefile a través de sus IDs
## Debieramos probar a hacer esto con spCbind
idx <- match(mapa$PROV,  datTotal$ID)

mapa$dat <- datTotal$dat[idx]

##Elaboro mapa. Elijo intervalos con classIntervals
library(classInt)
library(colorspace)
int <- classIntervals(mapa$dat, 10, style='jenks')
cols <- heat_hcl(10)
spplot(mapa['dat'], col.regions=cols, at=int$brks)

####################################################################################

## Ejemplo de Emilio
ficheropx <- read.px(filename="pcaxis824627184.px")
summary(ficheropx)
nombremapa <- ficheropx$MAP
nombremapa
nombremapasincomillas <- gsub("\"","", nombremapa) ## unquote de read.px
nombremapasincomillas ## humm, es un .ini

## http://www.ine.es/ss/Satellite?L=0&c=Page&cid=1254735116596&p=1254735116596&pagename=ProductosYServicios%2FPYSLayout
## hay que que ver los mapas que hay en el .ini
## [MAPS]
## NumMaps=2
## 1=C:\Archivos de programa\PX-Map\Maps\spain_provinces_img_ind_4.shp
## 2=C:\Archivos de programa\PX-Map\Maps\spain_regions_img_ind.shp
filename <- paste(nombremapasincomillas,".ini", sep="")
encoding = "latin1"
a <- scan(filename, what = "character", sep = "\n", quiet = TRUE, 
        fileEncoding = encoding)
## ¿qué lineas tienen un .shp?
lineasconshp <- a[grepl(".shp",a, ignore.case=TRUE)]
lineasconshp
## ¿Cómo sacamos los ficheros shp?
separamos <- unlist(strsplit(lineasconshp,"Maps\\",fixed=TRUE))
ficherosshp<- separamos[grepl(".shp",separamos, ignore.case=TRUE)]
ficherossinextension <- sub("^([^.]*).*", "\\1", ficherosshp)
## Dibujamos los mapas
library(maptools)
ficherossinextension[1]
ficherossinextension[2]
mapa1 <- readShapePoly(ficherossinextension[1])
mapa2 <- readShapePoly(ficherossinextension[2])

## Error en read.dbf(filen) : unable to open DBF file
# en readShapePoly llama a read.shape
## read.shape(file = ficherossinextension[1])
## Error en read.dbf(filen) : unable to open DBF file
## read.dbf(ficherossinextension[1])
# Error en read.dbf(ficherossinextension[1]) : unable to open DBF file
## read.dbf(paste(ficherossinextension[1],".dbf",sep="")) # OK, hay que añadirle la extensión

  
