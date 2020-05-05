\name{BlankPlot}
\alias{BlankPlot}

\title{
Generate a blank plot window
\subsection{}{
     \if{html}{\figure{DiPalm.png}{}}
    \if{latex}{}
  }
}
\description{
Creates a blank plotting window of desired size.
}
\usage{
BlankPlot(xrng = c(0, 1), yrng = c(0, 1), Main = "", xlab = "", ylab = "", ...)
}

\arguments{
  \item{xrng}{
The range of the x-axis.
}
  \item{yrng}{
The range of the y-axis.
}
  \item{Main}{
The main plot title.
}
  \item{xlab}{
X-axis label.
}
  \item{ylab}{
Y-axis label.
}
  \item{\dots}{
Additional arguments passed to the \code{\link[graphics]{plot}} function.
}
}
\details{
This function is used to initiate a blank plotting window in R. xrng and yrng are used to initiate the size of the plotting region.
}
\value{
Generates a blank plot
}

\author{
Ryan C. Sartor
}

\seealso{
\code{\link[graphics]{plot}}
}
\examples{
BlankPlot(xrng=c(0,10),yrng=c(0,10),Main="Test",xlab="xlab",ylab="ylab")
}