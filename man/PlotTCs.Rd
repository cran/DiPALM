\name{PlotTCs}
\alias{PlotTCs}

\title{
    \if{html}{\figure{DiPalm.png}{}}
    \if{latex}{}
Plot multiple time-course expression patterns for one gene
}
\description{
A plotting function that generates a line plot for a single gene in multiple time-course expression sets. Each time-course is overlaid on the same plot.
}
\usage{
PlotTCs(TClst, tgene, main = "", scale = TRUE, xlab = "Time", 
    ylab = "", xAxsLabs = colnames(TClst[[1]]), ledgeX = "top", 
    colAdj = 0.4, tcols = c("red", "red", "blue", "blue"), tltys = c(1, 1, 2, 2))
}

\arguments{
  \item{TClst}{
A named list of time-course matrices. Each element of the list is a time-course expression matrix (Genes as rows and time points as columns).
}
  \item{tgene}{
A string of a single gene accession (found in the row names of one element of TClst).
}
  \item{main}{
Main figure title.
}
  \item{scale}{
Logical: should the expression values of each time-course be scaled and centered?
}
  \item{xlab}{
The X-axis label.
}
  \item{ylab}{
The Y-axis label.
}
  \item{xAxsLabs}{
A vector of names to be associated with each x-axis point.
}
  \item{ledgeX}{
The x value from \code{\link[graphics]{legend}} function. Determines where the legend is plotted.
}
  \item{colAdj}{
Controls the alpha channel for the line colors using \code{\link[grDevices]{adjustcolor}}.
}
  \item{tcols}{
A vector of colors controlling the colors of the lines. Must list one color for each time-course in TClst.
}
  \item{tltys}{
A vector of integers controlling the line type (lty) of the lines. Must list one lty for each time-course in TClst.
}
}
\details{
This function is used to view the expression pattern of a single gene across multiple time-course samples.
}
\value{
The function generates a plot with multiple lines.
}

\author{
Ryan C. Sartor
}

\examples{
data(testData)
PlotTCs(TClst = testData$timeCourseList, tgene = "BraA05g36370R" ,
scale = TRUE,tcols = c("red","red","blue","blue"), tltys = c(1,2,1,2), ledgeX = "topleft")
}
