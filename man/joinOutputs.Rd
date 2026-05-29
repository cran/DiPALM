\name{joinOutputs}
\alias{joinOutputs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Joins outputs from multiple invocations of the function \code{\link{dipalm}}.
}
\description{
Given a collection of RData files containing the output of multiple invocations of the function \code{\link{dipalm}} this function combines them into a single set of results.
}
\usage{
joinOutputs(files, p, n, .log = open_log())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{files}{
A vector or list of RData files containing the outputs to be joined.
%%     ~~Describe \code{files} here~~
}
  \item{p}{
The \eqn{p}{p}-value to use when calling significance.
%%     ~~Describe \code{p} here~~
}
  \item{n}{
Genes must be called significant in at least \code{n} iterations to be called significant in the return value.
%%     ~~Describe \code{n} here~~
}
  \item{.log}{
Function used to write log statements.
%%     ~~Describe \code{.log} here~~
}
}

\value{
A list with the following components.
\item{sig.genes}{Vector of genes called significant with respect to AdjkMEs at the given \eqn{p}{p}-value in at least \code{n} iterations.}
\item{sig.MedGenes}{Vector of genes called significant with respect to AdjkMEds at the given \eqn{p}{p}-value in at least \code{n} iterations.}
\item{patternCor}{Average of the matrices named \code{patternCor} that were returned by the individual invocations of the function \code{\link{dipalm}}.}
\item{AdjkMEs}{Matrix of the AdjkME values. Columns correspond to the indiviual invocations of the function \code{\link{dipalm}} and rows correspond to genes.}
\item{AdjMEd}{Matrix of the AdjkMEd values. Columns correspond to the indiviual invocations of the function \code{\link{dipalm}} and rows correspond to genes.}
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


\examples{\donttest{
expr.file<-tempfile();
dipalm.file.1<-tempfile();
dipalm.file.2<-tempfile();
data('smallTestData')
write.csv(smallTestData,file=expr.file);
tmp<-dipalm(
	expr.file,
	7,
	parallel::detectCores(),
	'[TREATMENT].[REPLICATE].[TIME]',
	'CTL',
	0.05,
	open_log()
);
#save each component of the list returned by the function dipalm
do.call(save,as.list(c(names(tmp),envir=as.environment(tmp),file=dipalm.file.1)))
tmp<-dipalm(
	expr.file,
	7,
	parallel::detectCores(),
	'[TREATMENT].[REPLICATE].[TIME]',
	'CTL',
	0.05,
	open_log()
);
#save each component of the list returned by the function dipalm
do.call(save,as.list(c(names(tmp),envir=as.environment(tmp),file=dipalm.file.2)))
results<-joinOutputs(c(dipalm.file.1,dipalm.file.2),0.05,2);
}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{misc}
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
