\name{\%when\%}
\alias{\%when\%}
\alias{guard}
\alias{guards}
\alias{isStrict}
\alias{\%isa\%}
\alias{\%hasa\%}
\alias{\%hasall\%}
\alias{isa}
\alias{hasa}
\alias{hasall}

\title{
  Add guards to a function to define the conditions when a child function will
  execute
}
\description{
  The '\%when\%' operator defines the conditions for execution for the given 
  function. To use a function within futile.paradigm, a guard must be defined
  for each child function, even if it's a default guard using TRUE. The
  'guard' function is an older form of the '\%when\%' operator and is
  deprecated.

  The 'guards' function provides introspection and displays guards defined for
  a function hierarchy. 

  The '\%isa\%' operator performs type checking. 
  The 'isa' function is analogous to the operator form but the arguments are 
  switched. This is deprecated in favor of the '\%isa\%' operator.
  

  The '\%hasa\%' operator checks whether an object contains a named value.
  The 'hasa' function is analogous to the operator form but the arguments are 
  switched.  This is deprecated in favor of the '\%hasa\%' operator.

  The '\%hasall\%' function is like '\%hasa\%' but returns the conjunction of 
  the '\%hasa\%' results.
  The 'hasall' function is analogous to the operator form but the arguments are 
  switched.  This is deprecated in favor of the '\%hasall\%' operator.
}
\usage{
child.fn \%when\% condition
guard(child.fn, condition, strict = TRUE)
guards(fn, inherits = TRUE)

argument \%isa\% type
argument \%hasa\% property
argument \%hasall\% property

isa(type, argument)
hasa(property, argument)
hasall(property, argument)
}
\arguments{
  \item{child.fn}{This is the function for which the guard is applied. It does
    not need to exist yet}
  \item{condition}{The conditions for dispatching to this function. This can 
    either be an expression, a function, or vector of functions. See Details
    for more information }
  \item{strict}{Whether strict matching of arguments should be used. See Details
    for more information}
  \item{fn}{The function to find guards for. If this is a child function, the
    parent function will be queried}
  \item{inherits}{If a function is passed that has no guards, whether to search
    for a parent function. Typically this is safe to leave as the default}
  \item{type}{A symbol or character describing the type to match}
  \item{argument}{The argument to match the type or properties with}
  \item{property}{The property or properties to look for within the object}
}
\details{
Guards provide a mechanism for dispatching function variants consistent with the
declarative style of functional programming. Using guards for dispatching
has many benefits: function variants are self-contained and focus on a single
task (separation of concerns), design-by-contract is inherent, data manipulation
is separate from computational logic, self-documenting.

The futile.paradigm makes adoption of guards simple. First an abstract function 
name is chosen as the top of a function hierarchy. This is the name of the
function that is called in actual code. 
All concrete variants are then guarded by a guard
statement followed by the actual function definition. The guard is declared
using the '\%when\%' operator:

  <abstract function>.<identifier> \%when\% (<logical expression>)

The guard statement
tells futile.paradigm under what conditions this particular function variant
should be called. This is defined by passing an expression to the guard
function that operates on the arguments of the concrete function (see Examples).
This expression is dynamically bound to an actual function at run-time.
Guards are naturally scoped based on the number of arguments in a function.
Hence only functions with the same number of arguments as were passed into the
parent function will be considered for dispatching. 

When using strict guards (the default behavior) named arguments passed into the
parent function must match the named arguments defined in the child function.
Hence, dispatching is defined by both the number of arguments and the matching
named arguments, which introduces a greater level of control in dispatching 
than otherwise possible. In general, the built-in argument matching will Do
the Right Thing w.r.t. unnamed arguments, although the first match will be the
one applied, which could result in unexpected behavior if one is careless.

When strict guards are disabled, then only argument length will be applied as a
precondition plus any conditions defined by the guards. In certain circumstances
this may be desired, although in general strict guards provides greater control.
Note that this feature is deprecated.

The order that the function guards are defined determines the order that 
functions are evaluated for satisfaction of guard criteria. This behavior is
typically called top-to-bottom, left-to-right evaluation. The left-to-right
evaluation only applies to the explicit form when multiple function guards are
defined in a vector. These details are important to understand as default 
functions defined too early will be greedy and no other criteria will be 
evaluated. 

Another important consideration is that using the ellipsis argument is not 
supported in futile.paradigm. This is by design as the functional programming
approach intentionally makes function arguments explicit, such that the
ellipsis argument should never be needed in a child function definition. 

The 'isStrict' function is an introspective function that indicates whether
the given child function has strict guards or not.

The 'isa' function is used to perform type checking. Calling the function checks
whether 'argument' is a 'type'. This is more than syntactic sugar 
in guard sequences as it adds a level of protective indirection when accessing
type information. Note that the preferred format is to supply a raw symbol to
isa, and following naming conventions, it is easy to infer the meaning from 
the syntax without undue clutter.

The 'hasa' function allows for duck-typing to check whether an argument has
a particular named property. This function also supports raw symbols as the
argument.

}
\value{
No value is returned for '\%when\%' since the operator is used purely for its 
side-effects.

The 'guards' function returns a list of guard functions for each child function
defined. This essentially shows the evaluation path that UseFunction will take.

'isStrict' returns a logical value.

'\%isa\%' returns a logical value indicating whether a variable is an instance of class


'\%hasa\%' returns a logical value indicating whether a property exists in an object

'\%hasall\%' returns the conjunction of all hasa results for an object. This is 
useful if a set of properties are required for execution.
}

\author{ Brian Lee Yung Rowe }
\note{ In general functions in FP do not have side-effects. This principle does
  not apply here since this function is used to implement the framework itself.
}

\seealso{ \code{\link{UseFunction}}, \code{\link{ensure}} }
\examples{
# Note that these are trivial examples for pedagogical purposes. Due to their
# trivial nature, most of these examples can be implemented more concisely
# using built-in R features.

# This is optional and only required when naming is ambiguous
#logarithm <- function(...) UseFunction('logarithm', ...)

# Abbreviated form (recommended)
# The expression must operate on arguments declared in the concrete function.
logarithm.1 \%when\% is.numeric(x)
logarithm.1 <- function(x) logarithm(x, exp(1))

# Defaults are applied on a per-argument length basis
logarithm.default1 \%when\% TRUE
logarithm.default1 <- function(x) logarithm(as.numeric(x))

# Explicit form (only necessary for special cases)
logarithm.base \%when\% function(x,y) is.numeric(x) && is.numeric(y)
logarithm.base <- function(x,y) log(x, base=y)

logarithm.default2 \%when\% TRUE
logarithm.default2 <- function(x,y) logarithm(as.numeric(x), as.numeric(y))

logarithm(100,10)
logarithm(5)

# View the function variants for this abstract function
guards(logarithm)

# In the futile.paradigm, the convention is to name types in PascalCase
a <- create(Apple, seeds=103)
isa(Apple,a)
hasa(c(seeds, stem), a)

}
\keyword{ methods }
\keyword{ programming }
