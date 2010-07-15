\name{guard}
\alias{guard}
\alias{guards}
\alias{register}
\alias{isStrict}
\alias{isa}

\title{
  Add guards to a function to define the conditions when a child function will
  execute
}
\description{
  The 'guard' function defines the conditions for execution for the given 
  function. To use a function within futile.paradigm, a guard must be defined
  for each child function, even if it's a default guard using TRUE.

  The 'guards' function provides introspection and displays guards defined for
  a function hierarchy. 

  The 'isa' function performs type checking.
}
\usage{
register(fn.name, where)
guard(child.fn, condition, strict = TRUE)
guards(fn, inherits = TRUE)
isa(type, argument)
}
\arguments{
  \item{child.fn}{This is the function for which the guard is applied. It does
    not need to exist yet}
  \item{condition}{The conditions for dispatching to this function. This must
    be a function that returns a boolean value or can be a vector of functions }
  \item{strict}{Whether strict matching of arguments should be used. See Details
    for more information}
  \item{fn}{The function to find guards for. If this is a child function, the
    parent function will be queried}
  \item{inherits}{If a function is passed that has no guards, whether to search
    for a parent function. Typically this is safe to leave as the default}
  \item{fn.name}{The name of the function to register}
  \item{where}{The position or environment to find the function }
  \item{type}{A character describing the type to match}
  \item{argument}{The argument to match type with}
}
\details{
Guards are naturally scoped based on the number of arguments in a function.
Hence only functions with the same number of arguments as were passed into the
parent function will be considered for dispatching.

When using strict guards (the default behavior) named arguments passed into the
parent function must match the named arguments defined in the child function.
Hence, dispatching is defined by both the number of arguments and the matching
named arguments, which introduces a greater level of control in dispatching 
than otherwise possible. In general, the built-in argument matching with Do
the Right Thing w.r.t. unnamed arguments, although the first match will be the
one applied, which could result in unexpected behavior if one is careless.

Note that when strict argumentss are applied, the names of the arguments in the
guard functions are irrelevant. The strict matching occurs at the level of the
actual functions and not the guard functions.

When strict guards are disabled, then only argument length will be applied as a
precondition plus any conditions defined by the guards. In certain circumstances
this may be desired, although in general strict guards provides greater control.

The order that the function guards are defined determines the order that 
functions are evaluated for satisfaction of guard criteria. This is a detail
that is important to understand as default functions defined too early will be
greedy and no other criteria will be evaluated. 

Another important consideration is that using the ellipsis argument is not 
supported in futile.paradigm. This is by design as the functional programming
approach is intentionally making function arguments explicit, such that the
ellipsis argument should never be needed in a child function definition. 

The 'isStrict' function is an introspective function that indicates whether
the given child function has strict guards or not.

The 'register' function is used to manually register environments that a 
function resides. This is necessary for package development where the calling
environment doesn't seem to be registered properly.

The 'isa' function is used to perform type checking. Calling the function checks
whether 'argument' is a 'type'. This is simply syntactic sugar for convenience
in guard sequences.

}
\value{
No value is returned for guard. This function is used purely for its 
side-effects.

The 'guards' function returns a list of guard functions for each child function
defined. This essentially shows the evaluation path that UseFunction will take.

'isStrict' returns a logical value.

'isa' returns a logical value.
}

\author{ Brian Lee Yung Rowe }
\note{ In general functions in FP do not have side-effects. This principle does
  not apply here since this function is used to implement the framework itself.
}

\seealso{ \code{\link{UseFunction}} }
\examples{
# Note that these are trivial examples for pedagogical purposes. Due to their
# trivial nature, most of these examples can be implemented more concisely
# using built-in R features.
logarithm <- function(...) UseFunction('logarithm', ...)

guard(logarithm.base, function(x,y) is.numeric(x) && is.numeric(y))
logarithm.base <- function(x,y) log(x, base=y)

guard(logarithm.default2, TRUE)
logarithm.default2 <- function(x,y) logarithm(as.numeric(x), as.numeric(y))

guard(logarithm.1, function(x) is.numeric(x))
logarithm.1 <- function(x) logarithm(x, exp(1))

# Defaults are applied on a per-argument length basis
guard(logarithm.default1, TRUE)
logarithm.default1 <- function(x) logarithm(as.numeric(x))

}
\keyword{ methods }
\keyword{ programming }
