\name{col_format_sub}
\alias{col_format_sub}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Substitute values in for the time, treatment, and replicate portion of a column format.
%%  ~~function to do ... ~~
}
\description{
Given a column format as a string this function substitutes each instance of \code{'[TREATMENT]'}, \code{'[REPLICATE]'} and \code{'[TIME]'} with the corresponding given values.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
col_format_sub(
	col.format = "[TREATMENT].[REPLICATE].[TIME]",
	tr = ".*",
	re = ".*",
	ti = ".*",
	escape = TRUE
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{col.format}{
A string used to describe the column format of your dataset whose samples are assumed to be organized along 3 axes: treatment, time and replicate.
The column format uses placeholders \code{'[TREATMENT]'}, \code{'[REPLICATE]'} and \code{'[TIME]'} to denote corresponding information.
The placeholders in the format must be separated, for instance a column format of \code{'[TREATMENT].[TIME][REPLICATE]'} will not behave as expected;
instead \code{'[TREATMENT].[TIME].[REPLICATE]'} would be appropriate.
This defaults to \code{STD_FORMAT}.
%%     ~~Describe \code{col.format} here~~
}
  \item{tr}{
The value to substitute for the placeholder \code{'[TREATMENT]'}.
%%     ~~Describe \code{tr} here~~
}
  \item{re}{
The value to substitute for the placeholder \code{'[REPLICATE]'}.
%%     ~~Describe \code{re} here~~
}
  \item{ti}{
The value to substitute for the placeholder \code{'[TIME]'}.
%%     ~~Describe \code{ti} here~~
}
  \item{escape}{
Whether to escape regex special characters in the arguments \code{tr}, \code{re} and \code{ti}.
%%     ~~Describe \code{escape} here~~
}
}
\value{
A new string with the placeholders \code{'[TREATMENT]'}, \code{'[REPLICATE]'} and \code{'[TIME]'} substituted.
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{re_esc}}
}
\examples{
column.names<-c(
	'CTL.ZT1.1',
	'CTL.ZT1.2',
	'FRZ.ZT1.1',
	'FRZ.ZT1.2',
	'CTL.ZT4.1',
	'CTL.ZT4.2',
	'FRZ.ZT4.1',
	'FRZ.ZT4.2'
)
col.format<-'[TREATMENT].ZT[TIME].[REPLICATE]'
#grab time 1 samples
time1.regex<-re_esc(col_format_sub(col.format=col.format, ti='1'));
column.names[grep(time1.regex,column.names)]

#remove replicate
time.trt.regex<-re_esc( col_format_sub(col.format=col.format,ti='(.*)',tr='(.*)') )
column.names<-sub(time.trt.regex,'\\1.ZT\\2',column.names)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{ utilities }
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
