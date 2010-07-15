\name{crud}
\alias{create}
\alias{create.default}
\alias{access}
\alias{update}
\alias{delete}
\title{ API for CRUD-like operations }
\description{
Provides a high-level API for abstracting CRUD operations for arbitrary objects.
}
\usage{
create(x, ...)
create.default(type, ...)
access(x, ...)
update(x, ...)
delete(x, ...)
}
\arguments{
  \item{x}{ An arbitrary object }
  \item{type}{ A string that represents a type }
  \item{\dots}{ Additional arguments to pass to dispatched functions }
}
\details{
  Adding to 'create' requires a minimal function definition as the harness is
  provided in the package. Typically a list is returned and the function
  defines any defaults needed. This is similar to the S4 style but is simpler
  and not a requirement for using the rest of futile.paradigm.
}
\value{
  'create' returns an object of the requested type.

  'access' returns an object of the requested type.

  'update' has no return value.

  'delete' has no return value, but the given object is removed from the environment.
}
\author{ Brian Lee Yung Rowe }

\examples{
create.car <- function(x, wheels=4) list(wheels=wheels)
create(car)
}
\keyword{ programming }
\keyword{ methods }
