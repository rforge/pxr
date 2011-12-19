library(pxR)
library(sp)
library(lattice)
library(latticeExtra)
library(maptools)
#library(classInt)
#library(colorspace)

source('read.px.ini.alt.R')
source('../pkg/R/as.data.frame.px.R')##nueva versión no incluida en 0.24



unquote <- function(x){
  gsub('\\"', "", x)
}

panel.polygonNames <- function (x, y, z, subscripts, at = pretty(z), shrink, labels = NULL, 
                                label.style = c("mixed", "flat", "align"),
                                ## contour = FALSE, region = TRUE,
                                ## col = add.line$col, lty = add.line$lty, lwd = add.line$lwd, 
                                ## cex = add.text$cex, font = add.text$font,
                                ## fontfamily = add.text$fontfamily, 
                                ## fontface = add.text$fontface, col.text = add.text$col,
                                ..., 
                                col.regions = regions$col, alpha.regions = regions$alpha, 
                                grid.polygons, sp.layout) {
  regions <- trellis.par.get("regions")
  numcol <- length(at) - 1
  numcol.r <- length(col.regions)
  col.regions <- if (numcol.r <= numcol) 
    rep(col.regions, length = numcol)
  else col.regions[floor(1 + (1:numcol - 1) * (numcol.r - 1)/(numcol - 
                                                              1))]
  zcol <- rep(NA, length(z))
  for (i in seq(along = col.regions)) zcol[!is.na(x) & !is.na(y) & 
                  !is.na(z) & z >= at[i] & z < at[i + 1]] <- i
  label.style <- match.arg(label.style)
  zcol <- as.numeric(zcol[subscripts])
  pls = slot(grid.polygons, "polygons")
  pO = slot(grid.polygons, "plotOrder")
  for (i in pO) {
    Srs <- slot(pls[[i]], "Polygons")
    pOi <- slot(pls[[i]], "plotOrder")
    id <- slot(pls[[i]], 'ID')
    for (j in pOi) {
      coords = slot(Srs[[j]], "coords")
      if (slot(Srs[[j]], "hole")) {
        bg = trellis.par.get()$background
        if (bg$col == "transparent") 
          fill = "white"
        else fill = bg$col
        alpha = bg$alpha
      }
      else {
        fill = col.regions[zcol[i]]
        alpha = alpha.regions
      }
      gp = gpar(fill = fill, alpha = alpha)##, col = col, 
##        lwd = lwd, lty = lty)
      grid.polygon(coords[, 1], coords[, 2], default.units = "native", 
                   gp = gp, name=paste('ID', id, sep=':'))
    }
  }
}

plot.px <- function(x, select, shpPath = 'shp',  encoding = "latin1", ...){##n=10, style='fisher', ...){

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
    p <- spplot(mapi[select], ..., panel=panel.polygonNames)## col.regions=cols, at=int$brks, ...)
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


library(gridSVG)


plot(datPX, select='Mujeres')
grobs <- grid.ls()
nms <- grobs$name[grobs$type == "grobListing"]
str(grid.get(nms[139]))
##idxNames <- grep('GRID.polygon', nms)
idxNames <- grep('ID:', nms)
IDs <- strsplit(nms[idxNames], 'ID:')
IDs <- sapply(IDs, function(x)as.numeric(x[2]))

mapName <- unique(unquote(datPX$MAP))[1]
mapsInfo <- read.pxini(filename = mapName, shpPath='shp')
mapSHP <- readShapePoly(mapsInfo$filesshp)

mapaDat <- as.data.frame(mapSHP)
mapaIDs <- as.numeric(rownames(mapaDat))
## pols <- slot(mapSHP, 'polygons') 
## pOrder <- slot(mapSHP, 'plotOrder')
## pOrder2 <- for(i in pOrder){
##   pOrderi <- slot(pols[[i]], 'plotOrder')
##   pID <- slot(pols[[i]], 'ID')
## print(pID)
## }

## pID <- sapply(pOrder, function(i, pols){
##   ord <- slot(pols[[i]], 'plotOrder')
##   id <- slot(pols[[i]], 'ID')
##   as.numeric(id)
##   }, pols)


## lapply(unique(nms), function(i){
lapply(unique(IDs), function(i){
  id <- paste('ID', i, sep=':')
  dat <- mapaDat[which(mapaIDs==i),]
  info <- paste('Prov: ', dat$PROV, ', Area=', dat$AREA/10^4, sep='')
  g <- grid.get(id)
  grid.garnish(id,
               onmouseover=paste("showTooltip(evt, '", info, "')"),
               onmouseout="hideTooltip()")
})
grid.script(filename="tooltip.js")

gridToSVG('garnished.svg')






