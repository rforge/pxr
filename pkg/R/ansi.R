oem2ansi<-function(strS)
# translada caracter acentuado de OEM a ANSI
{
   cANSI<-("áéíóúàèìòùâêîôûñÑçÇÁÉÍÓÚÀÈÌÒÙÂÊÎÔÛ")
   cOEM<-(" ‚¡¢£…Š•—ƒˆŒ“–¤¥‡€µÖàé·ÔÞãë¶Ò×âê")
   return(chartr(cOEM,cANSI,strS))
}
ansi2oem<-function(strS)
# translada caracter acentuado de OEM a ANSI
{
   cANSI<-("áéíóúàèìòùâêîôûñÑçÇÁÉÍÓÚÀÈÌÒÙÂÊÎÔÛ")
   cOEM<-(" ‚¡¢£…Š•—ƒˆŒ“–¤¥‡€µÖàé·ÔÞãë¶Ò×âê")
   return(chartr(cANSI,cOEM,strS))
}
