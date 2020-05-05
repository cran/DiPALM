PlotTCs <-
function(TClst, tgene, main="", scale=TRUE, xlab="Time", ylab="", xAxsLabs = colnames(TClst[[1]]), ledgeX="top", colAdj=0.4, tcols=c("red","red","blue","blue"), tltys=c(1,1,2,2))
{
  # if ylab is not defined, use "Gene Expression" if not scaled and "Normalized Expression" if scaled
  if(ylab=="")
  {
    if(scale)
      ylab="Normalized Expression"
    else
      ylab="Gene Expression"
  }
  
  tcols<-adjustcolor(col = tcols, alpha.f = colAdj)
  tmat<-sapply(TClst,function(x) x[tgene,])
  
  # column normalize expression matrix
  if(scale)
    tmat<-apply(tmat,2,function(x) (x-mean(x,na.rm = T))/sd(x,na.rm = ))
  
  # calc the averages based on tltys
  tmeans<-tapply(1:ncol(tmat), INDEX = tltys, function(x) apply(tmat[,x,drop=F],1,mean))
 
  trng<-range(tmat)
  trng[2]<-trng[2]+(0.15*diff(range(tmat)))
  
  plot(tmat[,1],type="l",col=tcols[1],lty=tltys[1],lwd=3,ylim=trng, xlab=xlab, ylab=ylab, main=main, xaxt="n")
  axis(side = 1, labels = xAxsLabs, at=1:length(xAxsLabs))
  for(c in 2:ncol(tmat))
  {
    lines(tmat[,c],col=tcols[c],lty=tltys[c],lwd=3)
  }
  for(m in 1:length(tmeans))
  {
    #lines(tmeans[[m]],col="black",lty=as.numeric(names(tmeans)[m]),lwd=3)
  }
  legend(x = ledgeX,legend = colnames(tmat),col = tcols, lty = tltys, lwd=3, bty="n")
}
