#################################
# Funcion grafica general
#################################

fv.matplot<- function (x,mDat,type="l",legend=NULL,xylegend=NULL,ljust=c(1,1),
                       lcex=.7,lwd=c(2,1,1,2,1),lty=c(1,2,3,1,5),col=c(1,4,3,2,5),
                       ylab=NULL, xlab=NULL,bg=NULL,main="",ylim=NULL,xlim=NULL,add = F,
                       cex.axis=NULL,cex.main=NULL,cex.lab=NULL,pch=NULL,
                       atx=NULL,aty=NULL,...) {
           # old.par <-par(mai=c(0.8,0.8,0.2,0.2),bg="gray85")
           matplot(x,mDat,type=type,lwd=lwd,lty=lty,col=col,xlab=xlab,ylab=ylab,
                   axes=F,cex.lab=cex.lab,ylim=ylim,xlim=xlim,add = add,pch=pch)
           # if (is.null(legend)) {legend <- dimnames(mDat)[[2]]}
           if (is.null(xylegend)) {xylegend  <- c(min(x),max(mDat,na.rm=T)) }
           if (is.null(atx)) {if (is.null(xlim)){atx=pretty(x)} else atx=pretty(xlim)}
           axis(1,at=atx,line=1,cex.axis=cex.axis)
           if (is.null(aty)) {if (is.null(ylim)){aty=pretty(mDat[!is.na(mDat)])} else aty=pretty(ylim)}
           axis(2,at=aty ,line=0.5,cex.axis=cex.axis,cex.lab=cex.lab)
           if (! is.null(legend)) {
             legend(xylegend[1],xylegend[2],legend=legend,lwd=lwd,
                  lty=lty,col=col,cex=lcex,yjust=ljust[2],xjust=ljust[1])
                 # text.width=max(strwidth(legend,cex=.5)) )
           }
           title(main=main,cex.main=cex.main) #
           grid(col="gray50")
           # par()<-old.par
       }


piramide <-function (m,main=NULL,prop=TRUE,col=c(5,6),xlim=NULL,sub=NULL)
# Función para dibujar una pirámide sencilla
# m    : matriz de población: edad en filas y sexo en columnas ( 1ª columna los hombres)
# prop : Determina que la pirámide gráfique números absolutos o relativos a 10^6 personas
# col  : vector de color de las barras
{
    if(prop) {
        m<-10^6*m/sum(m)
        if (is.null(xlim)) xlim=c(-10000,10000)
        if (is.null(sub))  sub='Cifras relativas a un millón de personas'
     }
     -1*m[,1]->m[,1]
     barplot(t(m),beside=T,horiz=T,space=c(-1,0),col=col,xlim=xlim,main=main,sub=sub)
     print(xlim)
 }
