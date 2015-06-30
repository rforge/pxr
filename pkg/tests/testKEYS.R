library(pxR)

my.px  <- read.px(system.file("extdata", "example.px", package = "pxR"))

my.df  <- as.data.frame(my.px)
my.px2 <- as.px(my.df)         

write.px(my.px, filename = 'tmpzzx1.px')  #  without KEYS
write.px(my.px, filename = 'tmpzzx2.px', keys = c( "municipios","edad"))  #  with KEYS
write.px(my.px2, filename = 'tmpzzx3.px')  

file.remove('tmpzzx1.px','tmpzzx2.px', 'tmpzzx3.px')
