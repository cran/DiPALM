BlankPlot <-
function(xrng=c(0,1),yrng=c(0,1), Main="", xlab="", ylab="", ...)
{
  graphics::plot(x = NA, y = NA, xlim = xrng, ylim = yrng, bty="n", ylab=ylab, xlab=xlab,axes = F,main=Main, ...)
}
