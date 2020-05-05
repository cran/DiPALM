PlotTCsRibbon <-
function(TClst, tgenes, main="", xlab="Time", ylab="", xAxsLabs = colnames(TClst[[1]]), scale=TRUE, alpha=0.1, tcols, tltys, splits=rep(1,length(TClst)))
{
  # if ylab is not defined, use "Gene Expression" if not scaled and "Normalized Expression" if scaled
  if(ylab=="")
  {
    if(scale)
      ylab="Normalized Expression"
    else
      ylab="Gene Expression"
  }
  
  # if multiple plots are desired (splits are defined), make sure main titles exist, if necessary, repeat the given titles
  if(length(main)<length(unique(splits)))
    main<-rep(main,length(unique(splits)))
  
  tcolsRib<-adjustcolor(tcols,alpha.f = alpha)
  
  tLst<-lapply(TClst,function(x) x[tgenes,,drop=F])
  tReps<-paste(tcols,tltys,sep="")
  
  names(tcolsRib)<-tReps
  
  if(scale)
    tLst<-lapply(tLst,function(x) t(apply(x,1, function(y) (y-mean(y,na.rm = T))/sd(y,na.rm = T))))
  
  # define the mean expression vectors
  # combine reps
  mLst<-tapply(1:length(tLst), INDEX = tReps, function(x) do.call(rbind,tLst[x]))
  names(mLst)<-tapply(names(tLst), INDEX = tReps, function(x) gsub("\\.R[[:digit:]]$","",x)[1])
  
  mLst<-lapply(mLst,function(x) apply(x,2,function(x) mean(x,na.rm = T)))
  tcols<-tapply(tcols, INDEX = tReps, function(x) x[1])
  tltys<-tapply(tltys, INDEX = tReps, function(x) x[1])
  splitsM<-tapply(splits, INDEX = tReps, function(x) x[1])
  
  # define the Quartiles
  mLst2<-tapply(1:length(tLst),INDEX = paste(tcolsRib,splits,sep=""), function(x) do.call(rbind,tLst[x]))
  
  qLst<-lapply(mLst2,function(x) apply(x,2,function(y) c((mean(y,na.rm = T)+sd(y,na.rm = T)),(mean(y,na.rm = T)-sd(y,na.rm = T)))))
  
  qLst<-lapply(qLst,function(x) cbind(c(x[1,],x[2,(ncol(x):1)]),c(1:ncol(x),ncol(x):1)))
  
  trng<-range(unlist(lapply(qLst,function(x) x[,1])))
  prng<-trng
  prng[2]<-trng[2]*1

  if(length(unique(splits))>1)
    {
    # Next two lines will reset the graphics parameters after this function call
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
    
    par(mfrow=c(length(unique(splits)),1),mar=c(3,3,1,0.5),mgp=c(2,1,0))
    }
  SpM<-split(1:length(mLst), as.factor(splitsM))
  splitsQ<-tapply(splits, INDEX = paste(tcolsRib,splits,sep=""), function(x) x[1])
  names(splitsQ)<-tapply(tcolsRib,INDEX = paste(tcolsRib,splits,sep=""), function(x) x[1])
  SpQ<-split(1:length(qLst), as.factor(splitsQ))
  
  for(sp in 1:(length(unique(splits))))
    {
    BlankPlot(xrng = c(1,(length(mLst[[1]]))), yrng = prng, xlab = xlab, ylab=ylab, Main = main[sp])
    sapply(SpQ[[sp]], function(x) polygon(x = qLst[[x]][,2], y = qLst[[x]][,1], col = names(splitsQ)[x], border = "white"))
    sapply(SpM[[sp]], function(x) lines(mLst[[x]], col=tcols[x], lty=tltys[x], lwd=3))
    axis(side = 1, labels = xAxsLabs, at=1:length(mLst[[1]]))
    axis(side = 2, at = pretty(trng))
    legend("topright",legend = names(mLst)[SpM[[sp]]],col = tcols[SpM[[sp]]], lty = tltys[SpM[[sp]]], lwd=3, bty="n")
  }
}
