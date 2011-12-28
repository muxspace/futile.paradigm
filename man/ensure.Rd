\name{\%must\%}
\alias{\%must\%}
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
fn.ref \%must\% condition
ensures(fn, inherits = TRUE, child = NULL)

}
\arguments{
  \item{fn.ref}{This is the function for which the assertion is applied. 
  Unlike with 'guard', the function must exist }
  \item{condition}{The conditions for dispatching to this function. This can 
    either be an expression, a function, or vector of functions. See Details
    for more information }
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

\seealso{ \code{\link{UseFunction}}, \code{\link{\%when\%}} }
\examples{
# Note that these are trivial examples for pedagogical purposes. Due to their
# trivial nature, most of these examples can be implemented more concisely
# using built-in R features.


# The expression must operate on arguments declared in the concrete function.
logarithm \%when\% is.numeric(x)
logarithm \%as\% function(x) logarithm(x, exp(1))

# Explicit form (only necessary for special cases)
logarithm \%when\% is.numeric(x)
logarithm \%also\% is.numeric(y)
logarithm \%must\% (! is.nan(result) && ! is.infinite(result))
logarithm \%as\% function(x,y) log(x, base=y)

logarithm \%when\% TRUE
logarithm \%as\% function(x,y) logarithm(as.numeric(x), as.numeric(y))

logarithm(5)

# Uses all arguments in assertion
f \%when\% (is.numeric(a) & is.numeric(b) & b > 1)
f \%must\% (result == a + b)
f \%as\% function(a,b) a + b

f(2,3)

# View the function variants for this abstract function
ensures(logarithm)

}
\keyword{ methods }
\keyword{ programming }
