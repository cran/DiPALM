\name{open_log}
\alias{open_log}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Open a log
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Returns a function, which is a wrapper around the function \code{\link{cat}}, to be used for logging.
}
\usage{
open_log(log_file = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{log_file}{A character vector giving the name of a file to write the output of the log function to.
%%     ~~Describe \code{log_file} here~~
}
}

\value{
A function that can be used to write to the log file passed to \code{open_log}.
This function is a wrapper for \code{cat} that sets the argument \code{file} to the value passed for the parameter \code{log_file}.
The function also writes a newline provided the argument \code{sep} was not set to be a newline (by default \code{sep} is a single space).
Calling the returned function with \code{NULL} as the first argument will close the log file.

If the value of \code{log_file} is \code{NULL} the returned function will print to standard output instead of writing to a file.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{
%%  ~~who you are~~
William Gustafson
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{cat}}
}
\examples{
logfile<-tempfile()
.log<-open_log(logfile)
.log('This string is written to the file `log`.');
.log(NULL); #close the file `log`
.log<-open_log()
.log('This string is written to standard output.');
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{ print }
\keyword{ IO & file } 
\keyword{ utilities }
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
\concept{ log }
% Use only one concept per line.
