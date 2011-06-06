## oem2ansi<-function(strS)
## # translada caracter acentuado de OEM a ANSI
## {
##    cANSI<-("áéíóúàèìòùâêîôûñÑçÇÁÉÍÓÚÀÈÌÒÙÂÊÎÔÛ")
##    cOEM<-(" ‚¡¢£…Š•—ƒˆŒ“–¤¥‡€µÖàé·ÔÞãë¶Ò×âê")
##    return(chartr(cOEM,cANSI,strS))
## }
## ansi2oem<-function(strS)
## # translada caracter acentuado de OEM a ANSI
## {
##    cANSI<-("áéíóúàèìòùâêîôûñÑçÇÁÉÍÓÚÀÈÌÒÙÂÊÎÔÛ")
##    cOEM<-(" ‚¡¢£…Š•—ƒˆŒ“–¤¥‡€µÖàé·ÔÞãë¶Ò×âê")
##    return(chartr(cANSI,cOEM,strS))
## }

oem2ansi <- function(x){
  iconv(x, 'CP437', 'latin1')
  }

ansi2oem <- function(x){
  iconv(x, 'latin1', 'CP437')
  }
