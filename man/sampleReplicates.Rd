\name{sampleReplicates}
\alias{sampleReplicates}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Samples replicates from a dataset without replacement.
%%  ~~function to do ... ~~
}
\description{
Given a dataset as a \code{matrix} or \code{data.frame} whose columns are samples grouped by treatments, times and replicates this function permutes the replicates which are typically arbitrary labels. 
Treatments and times of each sample are fixed.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sampleReplicates(gene.expr, col.format, .log)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gene.expr}{
A \code{matrix} or \code{data.frame} containing the data to be permuted, typically this is gene expression data from a timecourse.
%%     ~~Describe \code{gene.expr} here~~
}
  \item{col.format}{
A column format string describing the formatting of the column names.
The format string uses placeholders, \code{'[TREATMENT]'}, \code{'[REPLICATE]'} and \code{'[TIME]'}.
%%     ~~Describe \code{col.format} here~~
}
  \item{.log}{
A function, returned by \code{\link{open_log}} to log information that can be helpful in debugging user code.
%%     ~~Describe \code{.log} here~~
}
}

\value{
A matrix containing the permuted dataset.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{
William Gustafson
%%  ~~who you are~~
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{col_format_sub}}
\code{\link{open_log}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
set.seed(132)
gene.expr<-matrix(1:24,nrow=3)
colnames(gene.expr)<-c(paste0('CTL.ZT1.',1:4),paste0('CTL.ZT4.',1:4))
sampleReplicates(gene.expr,'[TREATMENT].ZT[TIME].[REPLICATE]',open_log());
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{ ts }
\keyword{ utilities }
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
