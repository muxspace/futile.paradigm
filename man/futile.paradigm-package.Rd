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
The futile.paradigm implements a function dispatching method based on the guard
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

Using the package is simple and where possible the syntax is meant to be
consistent with UseMethod for ease of adoption. The base, or parent, function 
is defined with a call to UseFunction. This establishes the necessary 
dispatching for the futile.paradigm.

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

In many ways R is a functional language. Futile.paradigm complements R's 
functional strengths to make it easier to develop software in this paradigm.

}
\author{
Brian Lee Yung Rowe

Maintainer: Brian Lee Yung Rowe <r@nurometic.com>
}
\references{
Some background on using guards and pattern matching:
  http://www.erlang.org/doc/reference_manual/functions.html#id2271076
  http://en.wikipedia.org/wiki/Guard_\%28computing\%29
}
\keyword{ package }
\seealso{
\code{\link{UseFunction}}, \code{\link{guard}}, \code{\link{AbuseMethod}}
}
\examples{
# Define the parent function that calls the dispatcher
abs.max <- function(...) UseFunction('abs.max', ...)

# By convention the guard is defined before the concrete function variant
guard(abs.max.recursive1, is.numeric(a) && length(a) > 1)
abs.max.recursive1 <- function(a) abs.max(a[1], abs.max(a[2:length(a)]))

guard(abs.max.recursive2, is.numeric(a) && length(a) == 1)
abs.max.recursive2 <- function(a) a

guard(abs.max.csv, is.character(a) && grep(',',a,fixed=TRUE) == 1) 
abs.max.csv <- function(a)
  abs.max(as.numeric(strsplit(a, ',', fixed=TRUE)[[1]]))

guard(abs.max.psv, is.character(a) && grep('|',a,fixed=TRUE) == 1) 
abs.max.psv <- function(a)
  abs.max(as.numeric(strsplit(a, '|', fixed=TRUE)[[1]]))

# The explicit form allows multiple guard functions
guard(abs.max.twoArg, c( function(a,b) is.numeric(a), function(a,b) is.numeric(b)) )
abs.max.twoArg <- function(a, b) max(abs(a), abs(b))

# Using a guard of TRUE acts as a default condition
guard(abs.max.twoArgDefault, TRUE)
abs.max.twoArgDefault <- function(a,b) abs.max(as.numeric(a), as.numeric(b))


# Test some output
abs.max(2,-3) # Calls abs.max.twoArg

abs.max("3","-4") # Calls abs.max.twoArgDefault

abs.max(3,"-4") # Calls abs.max.twoArgDefault

a <- rnorm(10)
abs.max(a) # Calls abs.max.recursive1

abs.max(c('12,-3,-5,8,-13,3,1,3')) # Calls abs.max.csv
}
