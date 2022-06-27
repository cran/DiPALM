\name{AdjustPvalue}
\alias{AdjustPvalue}

\title{
\if{html}{\figure{DiPalm.png}{}}
\if{latex}{}
Determine p-values based on permutation test results
}
\description{
Used to calculate the false discovery rate associated with a specified cutoff. It uses a vector of actual test scores and a vector of permuted scores. The result can be considered a corrected p-value or q-value.
}
\usage{
AdjustPvalue(tVal, tVec, pVec)
}
\arguments{
  \item{tVal}{
The threshold or cutoff value to use (given this value, the function returns the FDR associated with using any score above this one as a positive result).
}
  \item{tVec}{
A numeric vector of actual test result values (used to determine true positives).
}
  \item{pVec}{
A numeric vector of permuted test result values (used to determine false positives).
}
}
\details{
This function assumes all test result values above the given threshold are true positives (TP) and all permuted result values above the threshold are false positives (FP). It calculates FDR as (FP)/(TP).
When applied to individual test results, this works to estimate an adjusted p-value (or q-value) for that result.
}
\value{
A numeric value representing the FDR associated with the given cutoff (tVal).
}
\author{
Ryan C. Sartor
}
\examples{
# Returns a vector of adjusted p-values from a vector of test results
data("testData")
AdjPval_kMEs<-sapply(testData$testResults[1:100],function(x) 
AdjustPvalue(tVal = x, tVec = testData$testResults, pVec = testData$permutedResults))
}
