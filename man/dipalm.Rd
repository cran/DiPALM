\name{dipalm}
\alias{dipalm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Run a single instance of the DiPALM pipeline with replicates permuted.
%%  ~~function to do ... ~~
}
\description{
Applys the DiPALM pipeline to a given set of gene expression data with replicates permuted within each timepoint.
The user may want to instead use the frontend function \code{\link{main}} which runs this function many times in parallel.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dipalm(
	expr.file,
	 wgcna.power,
	 threads,
	 col.format,
	 ctrl.treatment,
	 p.value,
	 .log = open_log()
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{expr.file}{
Name of a csv file containing the gene expression data.
It is assumed the first column consists of rownames,which are indexed by genes, and the columns are indexed by samples.
%%     ~~Describe \code{expr.file} here~~
}
  \item{wgcna.power}{
Softthresholding power used with WGCNA.
%%     ~~Describe \code{wgcna.power} here~~
}
  \item{threads}{
Number of threads to use when creating WGCNA modules.
%%     ~~Describe \code{threads} here~~
}
  \item{col.format}{
String describing the column format of the data.
The column format uses placeholders \code{'[TREATMENT]'}, \code{'[REPLICATE]'} and \code{'[TIME]'} to denote the location of the corresponding fields in the format.
%%     ~~Describe \code{col.format} here~~
}
  \item{ctrl.treatment}{
Name of the control treatment.
%%     ~~Describe \code{ctrl.treatment} here~~
}
  \item{p.value}{
A \eqn{p}{p}-value threshold used for calling genes as exhibiting differential pattern of expression, with and without regarding magnitude.
%%     ~~Describe \code{p.value} here~~
}
  \item{.log}{
Function to write log statements to, e.g. the output of the function \code{\link{open_log}}.
%%     ~~Describe \code{.log} here~~
}
}

\value{
A list with the following components:
\item{AdjkMEs }{Vector of adjusted kME \eqn{p}{p}-values which represent significance of differential pattern of expression with no regard for magnitude.}
\item{AdjMed }{Vector of adjusted kMed \eqn{p}{p}-values which represent significance of differential pattern of expression considering magnitude.}
\item{patternCor }{A matrix of correlations between genes called significant with respect to the AdjkMEs. This correlation matrix is used downstream as a distance measure in hierarchical clustering.}
\item{args }{The list of arguments this function was called with along with a hash of the gene expression matrix for logging purposes.}
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
data('smallTestData')
write.csv(smallTestData,file=expr.file);

dipalm(
	expr.file,
	7,
	parallel::detectCores(),
	'[TREATMENT].[REPLICATE].[TIME]',
	'CTL',
	0.05,
	open_log()
);
}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{ tc }
\keyword{ models }
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
\concept{ differential expression }
\concept{ pattern of expression }
\concept{ differential pattern of expression }
