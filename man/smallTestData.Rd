\name{smallTestData}
\alias{smallTestData}
\docType{data}
\title{
Small test data for the DiPALM pipeline.
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
A selection of 250 Arabidopsis Thaliana transcript profiles from a timecourse study on cold acclimation.
When applying the DiPALM pipeline roughly 100 genes should be called significant at a \eqn{p}{p}-value of \eqn{0.05}.
Furthermore, the genes called significant are expected to be concentrated within the last 125 genes.
}
\usage{data("smallTestData")}
\format{
  A data frame with 250 observations on the 48 variables.
Observations, which correspond to genes, are labeled by locus ids, e.g. AT5G64650.
Variables, which correspond to samples, are labeled in the format \code{[TREATMENT].[REPLICATE].[TIME]}.
There are two treatments, \code{CTL} and \code{FRZ}, four replicates 1,2,3,4 and 6 times 1,5,9,13,17,21.
}



\examples{\donttest{
file<-tempfile();
data("smallTestData")
write.csv(smallTestData, file=file)
results<-dipalm(file, 7, 2, "[TREATMENT].[REPLICATE].[TIME]","CTL",0.05);
sum(results$AdjkMEs<0.05);
## maybe str(x) ; plot(x) ...
}}
\keyword{datasets}
