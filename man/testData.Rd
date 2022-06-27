\name{testData}
\alias{testData}
\docType{data}
\title{
    \if{html}{\figure{DiPalm.png}{}}
    \if{latex}{}
Test Data: Data for function testing
}
\description{
A list of objects that can be used to test individual DiPALM functions.
}
\usage{data("testData")}
\format{
  A List of 6 objects:
  \enumerate{
    \item{$timeCourseList - A List of 4 matrices containing mRNA expression data on \emph{Brassica rapa} R500.}
      \itemize{
      \item{$Drought.R1 (Plants exposed to drought, Replicate 1)
        A matrix of time-course gene expression data [18428 Genes X  12 Time-points]}
      \item{$Drought.R2 (Plants exposed to drought, Replicate 2)
        A matrix of time-course gene expression data [18428 Genes X  12 Time-points]}
      \item{$Watered.R1 (Properly watered plants, Replicate 1)
        A matrix of time-course gene expression data [18428 Genes X  12 Time-points]}
      \item{$Watered.R2 (Properly watered plants, Replicate 2)
        A matrix of time-course gene expression data [18428 Genes X  12 Time-points]}
      }
    \item{$moduleEigengenes - A dataframe [12 rows and 90 columns] representing 90 different expression vectors that describe descrete expression patterns found in the whole dataset ($timeCourseList).}
      \itemize{
      \item{Rows: each row represents a single time-point}
      \item{Columns: each column represents a distinct expression pattern seen repeatedly in the full data}
      }
    \item{$modelDesign - A matrix [4 rows and 2 columns] used as a design matrix for a simple linear model. This matrix provides a mapping between the input instances (timeCourseList) and the two fixed effects fitted in the mode (Drought and Watered). This matrix was created with \code{\link[stats]{model.matrix}}.}
    \item{$modelContrast - A character string specifying the contrast to be evaluated using the fitted model for each gene. This example is used to compare watered to drought conditions.}
    \item{$testResults - A named numeric vector with 18428 values from DiPALM test results on actual data.}
      \itemize{
      \item{names: Gene accessions}
      \item{values: The summed absolute values of t-values generated from limma model contrasts related to each eigengene pattern. This can be thought of as a DiPALM score for differential patterning.}
      }
    \item{$permutedResults- A named numeric vector with 18428 values from DiPALM test results on permuted data. This vector acts as a null distribution in order to estimate significance of actual test results.}
      \itemize{
      \item{names: Gene accessions}
      \item{values: The summed absolute values of t-values generated from limma model contrasts related to each eigengene pattern using permuted expression data. This can be thought of as a null distribution of DiPALM scores for differential patterning.}
  }
}
}

\source{
Data is from: \href{https://elifesciences.org/articles/29655}{Greenham et al., eLife 2017;6:e29655}
}
