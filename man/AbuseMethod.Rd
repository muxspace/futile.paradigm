\name{AbuseMethod}
\alias{AbuseMethod}
\title{ Dispatcher for high-level API functions }
\description{
  Used for library authors defining very high-level API functions. Not typically
  needed for normal development.
}
\usage{
AbuseMethod(fn.name, type, ..., EXPLICIT = FALSE, ALWAYS = TRUE)
}
\arguments{
  \item{fn.name}{ The parent function name. This is just the name of the 
    original function}
  \item{type}{ The target type or a regular value from which the type is derived }
  \item{\dots}{ Arguments to pass to the dispatched function }
  \item{EXPLICIT}{ Whether the type is passed in explicitly or not }
  \item{ALWAYS}{ Whether a default function should be called if all else fails }
}
\details{
This alternative dispatching is for specialized purposes. It allows certain
syntactic sugar not possible in UseMethod when performing method dispatching.
If none of the above made sense, then don't use this function. Otherwise, it
can be useful when defining very high-level functions that define interfaces
for an API.

In the future this function may take on additional functionality to manage
dispatching certain functions based on computer/network architecture.
}
\value{ Returns the result of the dispatched function. When ALWAYS is FALSE,
no default function will be called. Instead, an error message is printed. In
certain circumstances this fail-fast behavior is preferred over the default
lenient behavior.}
\author{ Brian Lee Yung Rowe }

\seealso{
\code{\link{UseMethod}}, \code{\link{UseFunction}}
}
\examples{
# Trivial example for pedagogical reasons only
product <- function(...) AbuseMethod('product', ...)

product.numeric <- function(a,b) a * b
product.matrix <- function(a,b) a %*% b

product(4,2)

}
\keyword{ methods }
\keyword{ programming }
