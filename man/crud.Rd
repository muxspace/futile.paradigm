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
  TODO
}
\value{
  'create' returns an object of the requested type.

  'access' returns an object of the requested type.

  'update' has no return value.

  'delete' has no return value, but the given object is removed from the environment.
}
\author{ Brian Lee Yung Rowe }

% \examples{
% }
\keyword{ programming }
\keyword{ methods }
