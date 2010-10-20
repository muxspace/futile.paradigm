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
create(type, ...)
create.default(type, ...)
}
\arguments{
  \item{type}{The object type to create as a symbol or character}
  \item{\dots}{ Additional arguments to pass to dispatched functions }
}
\details{
  Adding to 'create' requires a minimal function definition as the harness is
  provided in the package. Typically a list is returned and the function
  defines any defaults needed. This is similar to the S4 style but is simpler
  and not a requirement for using the rest of futile.paradigm.

  In general the futile.paradigm avoids strings where syntax is
  explicit enough that this is possible. By convention types are PascalCased,
  which makes identifying a type even clearer. For this to work properly, it
  is essential that class names are not defined as types as defined symbols
  are assumed to be valid for dereferencing. 
}
\value{
  'create' returns an object of the requested type.
}

\author{ Brian Lee Yung Rowe }

\examples{
create.Car <- function(T, wheels=4, doors=4) list(wheels=wheels, doors=doors)

# This is how you inherit from a type
create.SportsCar <- function(T, doors=2, ...)
  create(Car, doors=doors, ...)

my.car <- create(Car, doors=5)

your.car <- create('SportsCar')

# The type can be passed in via a variable as well
her.class <- 'Car'
her.car <- create(her.class, wheels=3)

my.car \%isa\% Car
your.car \%isa\% SportsCar
}
\keyword{ programming }
\keyword{ methods }
