\name{msub}
\alias{msub}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Regex substitution applied to a vector or list.
%%  ~~function to do ... ~~
}
\description{
A convenient frontend for the function \code{\link{sub}} that applies the function to each element of a vector or list and returns the result in the same form.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
msub(regex, repl, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{regex}{
Regular expression to use for matching.
%%     ~~Describe \code{regex} here~~
}
  \item{repl}{
Replacement string.
%%     ~~Describe \code{repl} here~~
}
  \item{\dots}{
The first variadic argument is the object to perform the regular expression substitution to.
This can be a vector or list of strings.
Any remaining arguments are passed to the function \code{\link{sub}}.
%%     ~~Describe \code{\dots} here~~
}
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A vector of strings resulting from applying the regular expression substutition to each component of the given vector.
}

\author{
William Gustafson
%%  ~~who you are~~
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{sub}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
msub("^CTL\\\\.(.*)\\\\.(.*)$","\\\\1.\\\\2",c("CTL.ZT1.1","FRZ.ZT1.1","CTL.ZT4.2","FRZ.ZT4.2"));
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
\keyword{ utilities }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
