\name{BuildModMembership}
\alias{BuildModMembership}

\title{
    \if{html}{\figure{DiPalm.png}{}}
    \if{latex}{}
Calculate module membership (kME) for all genes in a time-course data set
}
\description{
This function calculates module membership (kME) between each gene's expression vector throughout a time-course and the module eigengenes of the data set (from the WGCNA package, using \code{\link[WGCNA:blockwiseModules]{blockwiseModules}}). Module membership is defined as the Pearson correlation between the expression of a single gene and the expression of a single eigengene. 
}
\usage{
BuildModMembership(MeMat, TCsLst)
}

\arguments{
  \item{MeMat}{
A data frame of eigengenes with each column being a different eigengene vector.
}
  \item{TCsLst}{
A named list of time-course matrices. Each element of the list is a time-course expression matrix (genes as rows and time-points as columns).
}
}
\details{
The matrix of eigengene vectors, \code{MeMat} (see \code{\link[WGCNA:blockwiseModules]{blockwiseModules}} from the WGCNA package) is used as a convenient way to summarize all the unique expression patterns (across time) that are present in a time-course data set. 
Any large gene expression data set from a complex organism will contain tens of thousands of genes. However, extensive coordinated regulation is present in all biological systems. Because of this co-regulation, usually only dozens of distinct expression patterns exist and most genes can be categorized by one (or a few) of them.
By using a relatively small set of descriptive expression vectors (eigengenes), we can "encode" the pattern of expression for any single gene into a length N vector where N is the number of eigengenes and the values of the vector are the Pearson correlation of the gene to each eigengene (kME).
These kME values can then be compared using the \code{\link[limma]{limma}} package to determine differential patterning similar to how expression values are compared to determine differential expression.
This function is the first step to calculate all kMEs for each gene in a list of time-course matrices relative to the set of eigengenes for the whole data set.

}
\value{
This function returns a list of kME matrices. The list contains a separate element for each eigengene. Each element is a matrix with a row for each gene and a column for each member of the input \code{TCsLst}. 
}

\author{
Ryan C. Sartor
}

\seealso{
\code{\link[WGCNA]{blockwiseModules}}
}
\examples{
data("testData")
kMEsList<-BuildModMembership(MeMat=testData$moduleEigengenes, TCsLst=testData$timeCourseList)
}

