ggPlotMultiDensities <-
function(denslist, main="", xlab="", ylab="Normalized Frequency",scale=T, cols=c("red","blue","grey50","black","skyblue","orange"), ledgx="topright", ltype=NULL, lwidth=NULL, xrng=NULL, yrng=NULL, Ledge=T)
{
  theme_set(theme_classic())
  denslist<-lapply(denslist,function(x) x[!is.infinite(x)])
  dlist<-lapply(denslist,function(x) density(x,na.rm = T))
  if(scale)
  {
    Nums<-sapply(denslist,length)
    Nums<-Nums/max(Nums)
    for(i in 1:length(dlist))
    {
      dlist[[i]]$y<-dlist[[i]]$y*Nums[i]
    }
  }
  
  if(is.null(xrng))
  {
    xrng<-range(unlist(sapply(dlist,function(x) x$x)))
    xrng<-c((xrng[1]-(xrng[1]*0.05)),(xrng[2]+(xrng[2]*0.05)))
  }
  if(is.null(yrng))
    yrng<-range(unlist(sapply(dlist,function(x) x$y)))
  
  
  ypret<-pretty(yrng)
  yrng<-c((yrng[1]-(yrng[1]*0.05)),(yrng[2]+(yrng[2]*0.05)))
  #pry<-pretty(yrng)
  #pry<-pry[-length(pry)]
  #plot(0,type="n",xlim=xrng,ylim=yrng,main=main,xlab=xlab,ylab=ylab,xaxs="i",yaxs="i",yaxt="n",bty="n")
  #axis(2,ypret)
  #axis(2,at = ypret, labels = rep("",length(ypret)))
  
  #cols<-c("red","blue","grey50","black","skyblue","orange")     #grey.colors(1, start = 0.7, end = 0.7))
  if(is.null(ltype))
    ltype<-1
  
  if(is.null(lwidth))
    lwidth<-0.5
  
  if(length(dlist)>length(cols))
  {
    efact<-ceiling(length(dlist)/length(cols))
    cols<-rep(cols,times=(efact))
    ltype<-rep(1:efact,each=length(ltype))
  }
  
  ddf<-lapply(names(dlist),function(n) data.frame(x=dlist[[n]]$x,y=dlist[[n]]$y,Populations=rep(n,times=length(dlist[[n]]$x))))
  ddf<-do.call(rbind,ddf)
  ggplot(data = ddf, mapping = aes_string(x = "x", y = "y"))+ labs(x= xlab, y= ylab, title=main) + geom_area(aes_string(fill="Populations"),alpha=1/(length(dlist)+1)) + geom_line(mapping = aes_string(color="Populations"),size=lwidth,linetype=ltype) + scale_color_manual(values=cols) + scale_fill_manual(values=cols) + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())
  #return(ddf)
}
