\name{futile.paradigm-package}
\alias{futile.paradigm-package}
\alias{futile.paradigm}
\alias{paradigm.options}
\docType{package}
\title{
A framework for working in a functional programming paradigm in R
}
\description{
Provides dispatching implementations suitable for functional 
programming paradigms. The framework provides a mechanism for attaching guards
to functions similar to Erlang, while also providing the safety of assertions
reminiscent of Eiffel.
}
\details{
\tabular{ll}{
Package: \tab futile.paradigm\cr
Type: \tab Package\cr
Version: \tab {version}\cr
Date: \tab {date}\cr
License: \tab LGPL-3\cr
LazyLoad: \tab yes\cr
}
In many ways R is a functional language. The futile.paradigm complements R's 
functional strengths to make it easier to develop software in this paradigm.
This package implements a function dispatching method based on the guard
concept (in lieu of direct pattern matching) for functional programming in R. 
In contrast to UseMethod, which is used for S3 object-oriented
method dispatching, futile.paradigm introduces UseFunction for functional
programming. Since UseMethod only detects the type of the first argument, 
dispatching can be tricky/cumbersome when multiple arguments and/or types are
supported. Also, the direct manipulation of class attributes is dangerous in
the S3 paradigm. In the futile.paradigm, access is restricted via encapsulation
to reduce the fragility of such a mechanism. In lieu of the S4 style that
requires significant up-front design, the futile.paradigm intends to ease
the development process from initial experimentation to systems development.
This package provides a richer syntax for defining functions
including the constraints that must be satisfied in order to dispatch to the
particular function. A happy consequence of this approach is that code becomes
self-documenting and functions are more concise since there is a clean
separation of concerns between the true logic of a function and the data
management tasks within the function.

Concrete implementations of the abstract function, must be defined in a
hierarchical naming scheme based on the name of the abstract function. 
Similar to UseMethod,
the concrete function name is derived from the abstract function with a unique 
suffix separated by a dot. Unlike UseMethod, the suffix is arbitrary as the 
dispatching and association is controlled by the guard and the number of 
arguments in the concrete function. Guards are expressions using the same
variables as the concrete function variant or explicit functions operating
on the same number of arguments as the concrete function it is guarding.
Either form of a guard must return a boolean value.
Multiple guard functions can be defined in the guard call and in this situation
all guards must resolve to TRUE for the function to execute. In addition to a
function, a default function can be defined by setting the guard expression
to TRUE.

By convention the guard is declared before the function definition as this 
communicates immediately the criteria required to execute this particular 
implementation of the parent function.  See the examples section for a trivial
implementation.

In addition to guard statements, this package provides post-assertions 
reminiscent of Eiffel's design-by-contract approach. Using 'ensure' 
declarations validates function outputs and fails if they aren't satisfied.
The arguments to a generated assertion are ordered with the result object
first (named 'result'), then all of the function arguments.

In many ways R is a functional language. Futile.paradigm complements R's 
functional strengths to make it easier to develop software in this paradigm.

Conventions:
. Types are named in PascalCase
. Functions are named in lower case with words separated by underscore
. Function namespaces are separated by dot
. Declare guard statements before concrete implementations
. Declare ensure statements after concrete implementations
. Avoid explicit UseFunction definitions
. Avoid default function implementations unless a package defines all possible
  variants

}
\author{
Brian Lee Yung Rowe

Maintainer: Brian Lee Yung Rowe <r@nurometic.com>
}
\references{
Some background on using guards and pattern matching:
  %http://www.erlang.org/doc/reference_manual/functions.html#id2271076
  http://en.wikipedia.org/wiki/Guard_\%28computing\%29
}
\keyword{ package }
\seealso{
\code{\link{UseFunction}}, \code{\link{guard}}, \code{\link{ensure}}
}
\examples{
# Optionally define the parent function that calls the dispatcher
#abs_max <- function(...) UseFunction('abs_max', ...)

# By convention the guard is defined before the concrete function variant
guard(abs_max.recursive1, is.numeric(a) && length(a) > 1)

# This adds a post-assertion to ensure the value is what you expect
ensure(abs_max.recursive1, result > 0)

abs_max.recursive1 <- function(a) abs_max(a[1], abs_max(a[2:length(a)]))

guard(abs_max.recursive2, is.numeric(a) && length(a) == 1)
ensure(abs_max.recursive2, result == a)
abs_max.recursive2 <- function(a) a

guard(abs_max.csv, isa(DataObject,a) & hasa(data,a))
abs_max.csv <- function(a) abs_max(as.numeric(strsplit(a$data, ',')[[1]]))

# The explicit form allows multiple guard functions
guard(abs_max.twoArg, c(
  function(a,b) is.numeric(a),
  function(a,b) is.numeric(b)) )
ensure(abs_max.twoArg, result >= a | result >= b)
abs_max.twoArg <- function(a, b) max(abs(a), abs(b))

# Using a guard of TRUE acts as a default condition
guard(abs_max.twoArgDefault, TRUE)
abs_max.twoArgDefault <- function(a,b) abs_max(as.numeric(a), as.numeric(b))

# Define constructor for DataObject
create.DataObject <- function(T, data, name=NA) list(name=name, data=data)

# Test some output
abs_max(2,-3) # Calls abs_max.twoArg

abs_max("3","-4") # Calls abs_max.twoArgDefault

abs_max(3,"-4") # Calls abs_max.twoArgDefault

a <- rnorm(10)
abs_max(a) # Calls abs_max.recursive1

b <- create(DataObject, c('12,-3,-5,8,-13,3,1,3'))
abs_max(b) # Calls abs_max.csv


## Newton-Raphson optimization
converged <- function(x1, x0, tolerance=1e-6) abs(x1 - x0) < tolerance
minimize <- function(x0, algo, max.steps=100)
{
  step <- 0
  old.x <- x0
  while (step < max.steps)
  {
    new.x <- iterate(old.x, algo)
    if (converged(new.x, old.x)) break
    old.x <- new.x
  }
  new.x
}

guard(iterate.nr, isa(NewtonRaphson, algo))
iterate.nr <- function(x, algo) x - algo$f1(x) / algo$f2(x)

guard(iterate.gd, isa(GradientDescent, algo))
iterate.gd <- function(x, algo) x - algo$step * algo$f1(x)

create.GradientDescent <- function(T, f1, step=0.01) list(f1=f1,step=step)


fx <- function(x) x^2 - 4
f1 <- function(x) 2*x
f2 <- function(x) 2

algo <- create(NewtonRaphson, f1=f1,f2=f2)
minimize(3, algo)

algo <- create(GradientDescent, f1=f1,step=0.1)
minimize(3, algo)
}
