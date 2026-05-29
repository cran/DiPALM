\name{make_TCs}
\alias{make_TCs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Break a data matrix into time courses separated by treatment and replicate.
%%  ~~function to do ... ~~
}
\description{
Given a data set split into treatment groups, replicates and timepoints this function separates the dataset by treatment groups and replicate labels.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
make_TCs(
	gene.expr,
	time_order = NULL,
	col.format = "[TREATMENT].[REPLICATE].[TIME]",
	.log = open_log()
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gene.expr}{
Input data set as a \code{matrix} or \code{data.frame}.
Rows should be indexed by genes and columns by samples.
%%     ~~Describe \code{gene.expr} here~~
}
  \item{time_order}{
Optional argument specifying the order of the timepoints in the returned matrices.
%%     ~~Describe \code{time_order} here~~
}
  \item{col.format}{
String describing the column format of the data.
The column format uses placeholders \code{'[TREATMENT]'}, \code{'[REPLICATE]'} and \code{'[TIME]'} to denote the location of the corresponding fields in the format.
%%     ~~Describe \code{col.format} here~~
}
  \item{.log}{
A function to write log statements to such as the return value of \code{\link{open_log}}.
%%     ~~Describe \code{.log} here~~
}
}

\value{
A list indexed by all pairs of a treatment and a replicate label from the input dataset.
Each component of the returned list is a matrix containing the samples from the corresponding treatment and replicate label pair.
The column names of these matrices are the timepoints from the input dataset.
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
\code{\link{open_log}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
gene.expr<-matrix(1:16,nrow=2)
colnames(gene.expr)<-c(
	'CTL.1.0',
	'FRZ.1.0',
	'CTL.1.4',
	'FRZ.1.4',
	'CTL.2.0',
	'FRZ.2.0',
	'CTL.2.4',
	'FRZ.2.4'
)
make_TCs(gene.expr,col.format='[TREATMENT].[REPLICATE].[TIME]',.log=open_log())
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{ tc }
\keyword{ utilities }
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
