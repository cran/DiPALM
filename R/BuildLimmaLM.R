BuildLimmaLM <-
function(dataMat, designMat, contrastStr)
{
  #require(limma)
  fit1<-lmFit(dataMat, designMat)
  ContMat<- makeContrasts(contrasts = contrastStr, levels = designMat)
  fit2<- contrasts.fit(fit = fit1, contrasts = ContMat)
  fit2 <- eBayes(fit2)
  return(fit2)
}
