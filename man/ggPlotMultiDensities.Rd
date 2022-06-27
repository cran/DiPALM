\name{ggPlotMultiDensities}
\alias{ggPlotMultiDensities}

\title{
    \if{html}{\figure{DiPalm.png}{}}
    \if{latex}{}
Plot multiple density distributions
}
\description{
This function generates one or more smoothed density distributions using \code{\link[ggplot2]{ggplot2}} functionality
}
\usage{
ggPlotMultiDensities(denslist, main = "", xlab = "", ylab = "Normalized Frequency",
 scale = T, cols = c("red", "blue", "grey50", "black", "skyblue", "orange"),
 ledgx = "topright", ltype = NULL, lwidth = NULL, xrng = NULL, yrng = NULL, Ledge = T)
}
\arguments{
  \item{denslist}{
A named list of populations. Each element is a vector of numbers. A separate density plot will be generated for each population.
}
  \item{main}{
The main plot title.
}
  \item{xlab}{
The x-axis label.
}
  \item{ylab}{
The y-axis label.
}
  \item{scale}{
When multiple densities are plotted, should they be scaled to represent the relative number of samples in each population.
}
  \item{cols}{
A vector of colors (one for each population in \code{denslist}).
}
  \item{ledgx}{
The x value from \code{\link[graphics]{legend}} function. Determines where the legend is plotted.
}
  \item{ltype}{
A vector of integers specifying the line types to be used for each population (repeated if necessary for multiple populations).
}
  \item{lwidth}{
The line width to be used in the plot.
}
  \item{xrng}{
A numeric vector of 2, the x-axis range to be plotted.
}
  \item{yrng}{
A numeric vector of 2, the y-axis range to be plotted.
}
  \item{Ledge}{
A logical: should the legend be plotted?
}
}
\details{
This function takes in a list of one or more numeric vectors. Each vector is considered a separate population and a smoothed density plot (similar to a histogram) will be generated for each and plotted together.
The actual numeric y-axis values are meaningless. A density plot is normalized so that the total area under the curve is equal to 1. 
When scale = T, the total area under the density plots may not equal one, but instead the total area is scaled to the relative proportions of the total population sizes of each population.
}
\value{
Generates multiple density plots on the same graph.
}

\author{
Ryan C. Sartor
}

\seealso{
\code{\link[ggplot2]{ggplot2}}
}
\examples{
data(testData)
require(ggplot2)

# Two populations of the same size, unscaled
ggPlotMultiDensities(denslist = list(Test=testData$testResults,Permuted=testData$permutedResults),
main = "Pattern Change Scores", xlab = "Differential Pattern Score",lwidth = 1)

# Two populations of different sizes, scaled
ggPlotMultiDensities(denslist = list(Test=testData$testResults[1:3000],
Permuted=testData$permutedResults), scale=TRUE , main = "Pattern Change Scores (Scaled)",
xlab = "Differential Pattern Score",lwidth = 1)

}

