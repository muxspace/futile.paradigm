\name{\%when\%}
\alias{\%when\%}
\alias{\%also\%}
\alias{\%as\%}
\alias{\%default\%}
\alias{\%isa\%}
\alias{\%hasa\%}
\alias{\%hasall\%}
\alias{guards}
\alias{variant}
\alias{rm.variant}

\title{
  Add guards to a function to define the conditions when a child function will
  execute
}
\description{
  The '\%when\%' operator defines the conditions for execution for the given 
  function. To use a function within futile.paradigm, a guard must be defined
  for each child function, even if it's a default guard using TRUE. Each
  '\%when\%' clause indicates a new function variant for a given function.

  The '\%also\%' operator specifies additional clauses that must be met to
  execute the given function variant. It is provided as a syntactic 
  convenience to shorten long guards.

  '\%as\%' is used to assign the function variant definition to the function

  '\%default\%' is used to specify a fallback if necessary

  The '\%isa\%' operator performs type checking. 

  The '\%hasa\%' operator checks whether an object contains a named value.

  The '\%hasall\%' function is like '\%hasa\%' but returns the conjunction of 
  the '\%hasa\%' results.

  The 'guards' function provides introspection and displays guards defined for
  a function hierarchy. 

  To access a specific function variant, use the 'variant' function. This 
  function takes a name as an argument, returning a function reference. The
  primary purpose of this is for debugging.
}
\usage{
fn.ref \%when\% condition
fn.ref \%also\% condition
fn.ref \%as\% fn.def

argument \%isa\% type
argument \%hasa\% property
argument \%hasall\% property

guards(fn.ref, inherits = TRUE)
variant(name.fn)

}
\arguments{
  \item{fn.ref}{The function for which a guard is applied. It does
    not need to exist yet}
  \item{fn.def}{An actual function definition to associate with the function
    variant}
  \item{condition}{The conditions for dispatching to this function. This can 
    either be an expression, a function, or vector of functions. See Details
    for more information }
  \item{inherits}{If a function is passed that has no guards, whether to search
    for a parent function. Typically this is safe to leave as the default}
  \item{type}{A symbol or character describing the type to match}
  \item{argument}{The argument to match the type or properties with}
  \item{property}{The property or properties to look for within the object}
  \item{name.fn}{A function name that references a specific function variant}
}
\details{
Guards provide a mechanism for dispatching function variants consistent with the
declarative style of functional programming. Using guards for dispatching
has many benefits: function variants are self-contained and focus on a single
task (separation of concerns), design-by-contract is inherent, data manipulation
is separate from computational logic, self-documenting.

The futile.paradigm makes adoption of guards simple. A function is decorated
with declarative statements, and futile.paradigm performs the necessary wiring.
Any number of function variants can be attached to a given function, with each
variant starting with the '\%when\%' operator:

  <function> \%when\% (<logical expression>)

The guard statement
tells futile.paradigm under what conditions this particular function variant
should be called. This is defined by passing an expression to the guard
function that operates on the arguments of the concrete function (see Examples).
This expression is dynamically bound to an actual function at run-time.
Guards are naturally scoped based on the number of arguments in a function.
Hence only functions with the same number of arguments as were passed into the
parent function will be considered for dispatching. 

Named arguments passed into the
parent function must match the named arguments defined in the child function.
Hence, dispatching is defined by both the number of arguments and the matching
named arguments, which introduces a greater level of control in dispatching 
than otherwise possible. In general, the built-in argument matching will Do
the Right Thing w.r.t. unnamed arguments, although the first match will be the
one applied, which could result in unexpected behavior if one is careless.

Each function variant has exactly one '\%when\%' clause. For convenience, 
additional conjunctive clauses (i.e. they all must resolve to true) can be 
attached to the variant with the '\%also\%' operator:

  <function> \%also\% (<logical expression>)

The order that the function guards are defined determines the order that 
functions are evaluated for satisfaction of guard criteria. This behavior is
typically called top-to-bottom, left-to-right evaluation. The left-to-right
evaluation only applies to the explicit form when multiple function guards are
defined in a vector. These details are important to understand as default 
functions defined too early will be greedy and no other criteria will be 
evaluated. 

Binding an actual function definition to this particular guard sequence is done
by using the \%as\% operator. Each variant needs at minimum a \%when\% statement
and an \%as\% statement to succesfully bind a new variant to the given function.

In addition to variants with guards, the \%default\% operator can be used to
specify a default function that is used if no function variants match when 
calling the function.

Another important consideration is that using the ellipsis argument is not 
supported in futile.paradigm. This is by design as the functional programming
approach intentionally makes function arguments explicit, such that the
ellipsis argument should never be needed in a child function definition. 

The '%isa%' operator is used to perform type checking. Using the operator checks
whether 'argument' is a 'type'. This is more than syntactic sugar 
in guard sequences as it adds a level of protective indirection when accessing
type information. Note that the preferred format is to supply a raw symbol to
%isa%, and following naming conventions, it is easy to infer the meaning from 
the syntax without undue clutter.

The '%hasa%' operator allows for duck-typing to check whether an argument has
a particular named property. This operator also supports raw symbols as the
argument.

Deprecated Functions
The below functions are for version 1 of the futile.paradigm and are officially
deprecated. They will be removed in future versions.

The 'isStrict' function is an introspective function that indicates whether
the given child function has strict guards or not.

When strict guards are disabled, then only argument length will be applied as a
precondition plus any conditions defined by the guards. In certain circumstances
this may be desired, although in general strict guards provides greater control.
Note that this feature is deprecated.

}
\value{
No value is returned for '\%when\%' nor '\%also\%' since these operators are
used purely for their side-effects.

The 'guards' function returns a list of guard functions for each child function
defined. This essentially shows the evaluation path that UseFunction will take.

The 'variant' function returns a function reference if found.

'\%isa\%' returns a logical value indicating whether a variable is an instance 
of class.


'\%hasa\%' returns a logical value indicating whether a property exists in an 
object.

'\%hasall\%' returns the conjunction of all '\%hasa\%' results for an object.
This is useful if a set of properties are required for execution.
}

\author{ Brian Lee Yung Rowe }
\note{ In general functions in FP do not have side-effects. This principle does
  not apply here since this function is used to implement the framework itself.
}

\seealso{ \code{\link{UseFunction}}, \code{\link{\%must\%}} }
\examples{
# Note that these are trivial examples for pedagogical purposes. Due to their
# trivial nature, most of these examples can be implemented more concisely
# using built-in R features.


# The expression must operate on arguments declared in the concrete function.
logarithm \%when\% is.numeric(x)
logarithm \%as\% function(x) logarithm(x, exp(1))

# Defaults are applied on a per-argument length basis
logarithm \%when\% TRUE
logarithm \%as\% function(x) logarithm(as.numeric(x))

logarithm \%when\% is.numeric(x)
logarithm \%also\% is.numeric(y)
logarithm \%as\% function(x,y) log(x, base=y)

logarithm \%when\% TRUE
logarithm \%as\% function(x,y) logarithm(as.numeric(x), as.numeric(y))

logarithm \%default\% function(...) cat("This is the default function\n")

logarithm(100,10)
logarithm(5)
logarithm("5")
logarithm(6,7,8)

# View the function variants for this abstract function
guards(logarithm)

# In the futile.paradigm, the convention is to name types in PascalCase
a <- create(Apple, seeds=103)
a %isa% Apple
a %hasa% c(seeds, stem)

}
\keyword{ methods }
\keyword{ programming }
