\name{BuildLimmaLM}
\alias{BuildLimmaLM}

\title{
    \if{html}{\figure{DiPalm.png}{}}
    \if{latex}{}
Fit linear models to time-course expression patterns using the limma package
}
\description{
This function fits a linear model (using \code{\link[limma]{lmFit}} form the limma package) to each set of kME values returned from \code{\link{BuildModMembership}}.
}
\usage{
BuildLimmaLM(dataMat, designMat, contrastStr)
}
\arguments{
  \item{dataMat}{
a matrix containing module membership values (a single element of the list returned by \code{\link{BuildModMembership}}). Module membership is a Pearson correlation value for a single gene compared to a module eigengene.
}
  \item{designMat}{
a linear model design matrix containing binary values (0 and 1) with rows as samples and columns as treatments. Use the \code{\link[stats]{model.matrix}} function in the stats package.
}
  \item{contrastStr}{
a linear model contrast specifying the desired differential comparison to be made using the linear model (see the \code{\link[limma]{limma}} package documentation for more details).
}
}
\details{
This function uses the limma package functionality to carry out an analysis of differential patterning. Asking if a gene displays a significantly different expression pattern across time when looking at one condition compared to another.
The concept of differential patterning is similar to differential expression. However, the expression level of most genes fluctuates significantly throughout the day. Most of this is due to diurnal or circadian rhythms. These regular changes in expression severely complicate differential expression analysis and give rise to the need for examining differential patterning.
By "encoding" the expression pattern of each gene into a vector of module membership (kME) values using the \code{\link{BuildModMembership}} function, the DiPALM analysis provides a way to use the limma method to examine differential patterning.

}
\value{
This function returns an \code{\link[limma]{MArrayLM-class}} object. This is a custom class from the \code{\link[limma]{limma}} package. It is an object that contains all information and results of individual linear models built for each gene in the input matrix.

}

\author{
Ryan C. Sartor
}

\seealso{
\code{\link{BuildLimmaLM}} , \code{\link[limma]{lmFit}} , \code{\link[limma]{makeContrasts}} , \code{\link[limma]{contrasts.fit}} , \code{\link[limma]{eBayes}}
}
\examples{
data("testData")
# First calculate the module memberships
kMEsList<-BuildModMembership(MeMat=testData$moduleEigengenes, TCsLst=testData$timeCourseList)

# Then build the limma models
LimmaMods<-lapply(kMEsList, function(x) 
BuildLimmaLM(dataMat = x, designMat = testData$modelDesign, contrastStr = testData$modelContrast))
}

