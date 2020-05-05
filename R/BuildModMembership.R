BuildModMembership <-
function(MeMat, TCsLst)
{
  # if the matrices have more rows than columns, transpose them
  tdim<-dim(TCsLst[[1]])
  if(tdim[1]>tdim[2])
    TCsLst<-lapply(TCsLst,function(x) t(x))
  
  kMELst<-lapply(TCsLst, function(x) cor(x,MeMat))
  
  mods<-colnames(MeMat)
  names(mods)<-colnames(MeMat)

  kMELst<-lapply(mods, function(x) do.call(cbind,lapply(kMELst,function(y) y[,x])))
  return(kMELst)
}
