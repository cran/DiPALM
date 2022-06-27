\name{PlotTCsRibbon}
\alias{PlotTCsRibbon}

\title{
    \if{html}{\figure{DiPalm.png}{}}
    \if{latex}{}
Plot multiple averaged time-course expression patterns for a group of genes
}
\description{
A plotting function that generates a ribbon plot representing the summarized expression of a set of genes. A separate ribbon is generated for each time-course. The ribbon represents + & - 1 standard deviation from the mean at each point.
}
\usage{
PlotTCsRibbon(TClst, tgenes, main = "", xlab = "Time", ylab = "",
 xAxsLabs = colnames(TClst[[1]]),scale = TRUE, alpha = 0.1, tcols, tltys,
 splits = rep(1, length(TClst)))
}

\arguments{
  \item{TClst}{
A named list of time-course matrices. Each element of the list is a time-course expression matrix (Genes as rows and time points as columns).
}
  \item{tgenes}{
A vector of gene accessions (found in the row names of one element of TClst).
}
  \item{main}{
The main title of the plot, if splits are defined, multiple plots will be generated and a vector of titles may be defined here.
}
  \item{xlab}{
The X-axis label.
}
  \item{ylab}{
The Y-axis label.
}
  \item{xAxsLabs}{
The time point labels for the x-axis, by default they will be the column names of the first time-course in TClst.
}
  \item{scale}{
Logical: should the expression values of each time-course be scaled and centered?
}
  \item{alpha}{
The alpha channel to be used for the ribbons (number from 0 - 1 defining transparency). See \code{\link[grDevices]{adjustcolor}}.
}
  \item{tcols}{
A vector of colors controlling the colors of the lines. Must list one color for each time-course in TClst.
}
  \item{tltys}{
A vector of integers controlling the line type (lty) of the lines. Must list one lty for each time-course in TClst.
}
  \item{splits}{
A vector of integers defining groups to be plotted on separate plots if desired (see examples).
}
}
\details{
This can be used to plot the summarized expression pattern of an entire cluster of genes and compare between multiple time courses. Line plots are generated representing the mean expression value at each time point for the group. Ribbons are plotted along the lines in semi-transparent colors. The ribbons represent + and - one standard deviation from the mean expression level of the group.
# 
}
\value{
This function generates multiple line plots representing the average expression of a group of genes with ribbons representing standard deviation. 
}

\author{
Ryan C. Sartor
}

\examples{
data("testData")

# Calculate the module memberships
kMEsList<-BuildModMembership(MeMat=testData$moduleEigengenes, TCsLst=testData$timeCourseList)

# Build the limma models
LimmaMods<-lapply(kMEsList, function(x) 
BuildLimmaLM(dataMat = x, designMat = testData$modelDesign, contrastStr = testData$modelContrast))

# Get the adjusted Pvalues for each gene
AdjPval_kMEs<-sapply(testData$testResults[rownames(LimmaMods[[1]])],function(x) 
AdjustPvalue(tVal = x, tVec = testData$testResults, pVec = testData$permutedResults))

# Cluster the genes base on the limma results
LimmaSums<-do.call(cbind,lapply(LimmaMods,function(x) x$t))
LimmaModsSig<-LimmaSums[names(AdjPval_kMEs)[which(AdjPval_kMEs<0.01)],]
patternCor<-cor(t(LimmaModsSig))
patternTree<-hclust(as.dist(1-patternCor),method = "complete")
patternClusters<-cutree(tree = patternTree, k = 25)
patternClusters<-tapply(rownames(patternCor),INDEX = patternClusters,function(x) x)

#Replicates are grouped together (same color + lty values)
PlotTCsRibbon(TClst = testData$timeCourseList, main="Drought Time-Course", 
xAxsLabs = c(seq(1,23,4),seq(1,23,4)), xlab="ZT Time (hours)", tgenes = patternClusters[[15]],
scale = TRUE, tcols = c("red","red","blue","blue"), tltys = c(1,1,1,1))

#Replicates are in different groups (same color but different lty values)
PlotTCsRibbon(TClst = testData$timeCourseList, main="Drought Time-Course",
xAxsLabs = c(seq(1,23,4),seq(1,23,4)), xlab="ZT Time (hours)", tgenes = patternClusters[[15]],
scale = TRUE, tcols = c("red","red","blue","blue"), tltys = c(1,2,1,2))

#Replicates are represented on differnt plots (using the splits argument)
PlotTCsRibbon(TClst = testData$timeCourseList, main=c("Rep1","Rep2"), 
xAxsLabs = c(seq(1,23,4),seq(1,23,4)), xlab="ZT Time (hours)", tgenes = patternClusters[[15]],
scale = TRUE, tcols = c("red","red","blue","blue"), tltys = c(1,2,1,2), splits = c(1,2,1,2))
}

