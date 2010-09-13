\name{UseFunction}
\alias{UseFunction}
\title{Primary dispatcher for functional programming }
\description{UseFunction is an alternative approach to function dispatching
vis-a-vis UseMethod. It is designed for people interested in writing functional
programs in R as opposed to object-oriented programs.
}
\usage{
UseFunction(fn.name, ...)
}
\arguments{
  \item{fn.name}{The name of a function that uses functional dispatching. This
    is just the name of the function being defined}
  \item{\dots}{The arguments that are passed to dispatched functions }
}
\details{
  In most situations (i.e. following the conventions outlined by the
  futile.paradigm), explicit use of UseFunction is unnecessary as the
  system will automatically generate and execute this command as necessary.

  Explicit function definitions follow a simple template:
    fn.var <- function(...) UseFunction('fn.var', ...)

  When calling the function, if no guards match, then an error is returned.
}
\value{
  Returns the value of the dispatched function
}
\author{ Brian Lee Yung Rowe }

\note{ For high-level API development, AbuseMethod may be more appropriate }

\seealso{
\code{\link{AbuseMethod}}
}
\examples{
# Note that these are trivial examples for pedagogical purposes. Due to their
# trivial nature, most of these examples can be implemented more concisely
# using built-in R features.

# Optional
#reciprocal <- function(...) UseFunction('reciprocal', ...)

guard(reciprocal.1, is.numeric(x) && x != 0)
reciprocal.1 <- function(x) 1 / x
ensure(reciprocal.1, sign(result) == sign(x))

guard(reciprocal.2, is.character(x))
reciprocal.2 <- function(x) reciprocal(as.numeric(x))

reciprocal(4)

}
\keyword{ methods }
\keyword{ programming }
