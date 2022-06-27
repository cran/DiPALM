\name{exampleData}
\alias{exampleData}
\docType{data}
\title{
    \if{html}{\figure{DiPalm.png}{}}
    \if{latex}{}
Example Data: Data for use with the DiPALM vignette
}
\description{
A list of data objects that are used to run through the DiPALM example vignette.
}
\usage{data("exampleData")}
\format{
  A List of 3 objects:
  \enumerate{
    \item{$rawCounts - A data frame [42383 rows X 48 columns] containing raw counts for mRNA expression data on \emph{Brassica rapa} R500.}
      \itemize{
      \item{rows : Each row represents the expression of one gene across many different samples.}
      \item{columns: Each column represents a different sample. Both drought-treated and properly watered samples are present, with two replicates of each time-course. Tissue was sampled at 4-hour intervals and column names are encoded as follows:
      S[replicate number] [D-Drought / W-Watered] [Time - sample]
      Example: S2D10 is part of the second replcate of the drought data and is the 10th time-point (ZT13, 13th hour after dawn on day two).}
      }
    \item{$moduleEigengenes - A dataframe [12 rows and 90 columns] representing 90 different expression vectors that describe descrete expression patterns found in the whole dataset ($timeCourseList).}
      \itemize{
      \item{Rows: each row represents a single time-point.}
      \item{Columns: each column represents a distinct expression pattern seen repeatedly in the full data.}
      }
    \item{$geneAnnotations - A data frame [42383 rows and 5 columns] giving the locations within the \emph{Brassica rapa} R500 genome where each gene is found. This is in SAF format which is used by the Linux Subread software package.}
      \itemize{
      \item{rows: Each row represents a single gene.}
      \item{$GeneID: (character string) Unique gene accession.}  
      \item{$Chr: (character string) Chromosome number.}  
      \item{$Start: (integer) The position where the gene starts (first basepair).}  
      \item{$End: (integer) The position where the gene ends (last basepair).}  
      \item{$Strand: (character) is the gene on the positive [+] or negative [-] strand.}  
      }
  }
}

\details{
Raw count data was summarized by mapping RNA-seq counts to the R500 \emph{Brassica rapa} genome using the hisat2 aligner. Mapped reads were counted using the featureCounts function from the Linux Subread software package.
}
\source{
Data is from: \href{https://elifesciences.org/articles/29655}{Greenham et al., eLife 2017;6:e29655}
}

