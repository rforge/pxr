library(pxR)

probar.px <- function(fichero){
	a <- read.px(fichero)
	b <- as.data.frame(a)
	b <- as.data.frame(a, direction = "wide")
	b <- as.data.frame(a, use.codes = T)
	b <- as.array(a)
	b <- as.array(a, use.codes = T)
}

a <- probar.px("example2.px")
a <- probar.px("example3.px")
a <- probar.px("example4.px")
a <- probar.px("example5.px")
