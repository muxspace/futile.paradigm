\name{\%must\%}
\alias{\%must\%}
\alias{ensure}
\alias{ensures}

\title{
  Add post-assertion validations to a function to define the conditions when 
  results of a child function are valid
}
\description{
  The 'ensure' function defines the conditions for successful execution for 
  the given function. This is an optional declaration that requires a previous
  guard declaration for the given function.

  The 'ensures' function provides reflection and displays assertions defined
  for a function hierarchy. 
}
\usage{
child.fn \%must\% condition
ensures(fn, inherits = TRUE, child = NULL)

ensure(child.fn, condition, strict = TRUE)
}
\arguments{
  \item{child.fn}{This is the function for which the assertion is applied. 
  Unlike with 'guard', the function must exist }
  \item{condition}{The conditions for dispatching to this function. This can 
    either be an expression, a function, or vector of functions. See Details
    for more information }
  \item{strict}{Whether strict matching of arguments should be used. See Details
    for more information}
  \item{fn}{The function to find assertions for. If this is a child function,
    the parent function will be queried}
  \item{inherits}{If a function is passed that has no assertions, whether to 
    search for a parent function. Typically this is safe to leave as the 
    default}
  \item{child}{Used to filter the assertions for a specific concrete function.
    Typically safe to ignore}
}
\details{
Combining guards with post-execution assertions provides a framework for 
design-by-contract programming. This paradigm forces developers to define
the programming interface for each function explicitly in the code. Doing 
so ensures that 
failures are detected immediately (known as fail-fast) reducing troubleshooting
time. Using the 'ensure' command is optional.

When using an expression in the ensure command, the executed function is
called with the following arguments: the result of the concrete function
(named 'result') followed by the arguments passed to the concrete function.
If the condition fails, execution will halt with an error message.
}
\value{
The '\%must\%' declaration is used for its side-effects and has no return value.

The 'ensures' function works like 'guards' and displays all registered 
assertions for a given abstract function.
}

\author{ Brian Lee Yung Rowe }

\seealso{ \code{\link{UseFunction}}, \code{\link{guard}} }
\examples{
# Note that these are trivial examples for pedagogical purposes. Due to their
# trivial nature, most of these examples can be implemented more concisely
# using built-in R features.

# This is optional except when naming is ambiguous
#logarithm <- function(...) UseFunction('logarithm', ...)

# Abbreviated form (recommended)
# The expression must operate on arguments declared in the concrete function.
logarithm.1 \%when\% is.numeric(x)
logarithm.1 <- function(x) logarithm(x, exp(1))

# Explicit form (only necessary for special cases)
logarithm.base \%when\% (is.numeric(x) && is.numeric(y))
logarithm.base \%must\% (! is.nan(result) && ! is.infinite(result))
logarithm.base <- function(x,y) log(x, base=y)

logarithm.default2 \%when\% TRUE
logarithm.default2 <- function(x,y) logarithm(as.numeric(x), as.numeric(y))

# Uses all arguments in assertion
f \%when\% (is.numeric(a) & is.numeric(b) & b > 1)
f \%must\% (result == a + b)
f <- function(a,b) a + b

# View the function variants for this abstract function
ensures(logarithm)

}
\keyword{ methods }
\keyword{ programming }
