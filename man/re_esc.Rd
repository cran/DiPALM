\name{re_esc}
\alias{re_esc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Format a string as a regular expression literal.
%%  ~~function to do ... ~~
}
\description{
Given a string returns escapes special characters used in regular expressions.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
re_esc(s)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s}{
String to be escaped.
%%     ~~Describe \code{s} here~~
}
}

\value{
A string with special characters escaped.
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


\examples{
re_esc('x.csv')
re_esc('abc')
re_esc('a\\\\(.*)\\\\')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
\keyword{ utilities }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
\concept{ regular expressions }
\concept{ escape }
