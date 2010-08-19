\name{crud}
\alias{create}
\alias{create.default}
\title{ API for CRUD-like operations }
\description{
Provides a high-level API for abstracting CRUD operations for arbitrary objects.
Currently only create is provided, as this is essential for type management
within futile.paradigm. Others will be added as necessary.
}
\usage{
create(x, ...)
create.default(type, ...)
}
\arguments{
  \item{x}{ An arbitrary object }
  \item{type}{ A symbol or character that represents a type }
  \item{\dots}{ Additional arguments to pass to dispatched functions }
}
\details{
  Adding to 'create' requires a minimal function definition as the harness is
  provided in the package. Typically a list is returned and the function
  defines any defaults needed. This is similar to the S4 style but is simpler
  and not a requirement for using the rest of futile.paradigm.

  Note that in general the futile.paradigm avoids strings where syntax is
  explicit enough that this is possible. By convention types are PascalCased,
  which makes identifying a type even clearer.
}
\value{
  'create' returns an object of the requested type.
}

\author{ Brian Lee Yung Rowe }

\examples{
create.Car <- function(x, wheels=4, doors=2) list(wheels=wheels, doors=doors)
my.car <- create(Car, doors=4)

isa(Car, my.car)
}
\keyword{ programming }
\keyword{ methods }
