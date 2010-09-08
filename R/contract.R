# Adds post-assertions to the base function
ensure <- function(child.fn, condition, strict=TRUE)
{
  child <- deparse(substitute(child.fn))

  expr <- deparse(substitute(condition))
  if (length(grep('function', expr)) < 1) 
    return(.ensure(child, expr, strict, label='ensure.xps'))

  return(.ensure(child, condition, strict, label='ensure.fns'))
}

# Shortcut form for expressions instead of more verbose functions
# This is the standard way of writing ensures, although the long form is
# slightly more efficient from an execution perspective
# label := { ensure.xps, ensure.fns }
.ensure <- function(child, condition, strict, label)
{
  # Need to add a lazy binding so that the function is bound only when
  # the actual child.fn is defined. Otherwise, it's impossible to determine
  # the arguments
  
  parent <- sub('\\.[^.]+$','', child)
  where <- paradigm.options(parent)
  if (is.null(where)) where <- -1

  if (! exists(parent, where))
  {
    msg <- "Function %s has no visible parent function '%s'"
    stop(sprintf(msg, child, parent))
  }
  fn <- get(parent, where)
  gs <- attr(fn, label)
  if (is.null(gs)) gs <- list()
  #gs[[child]] <- c(gs[[child]], expr)
  gs[[child]] <- c(condition)
  attr(fn, label) <- gs

  if (strict)
  {
    ss <- attr(fn, 'strict')
    if (is.null(ss)) ss <- list()
    ss[[child]] <- strict
    attr(fn, 'strict') <- ss
  }

  assign(parent, fn, where, inherits=TRUE)

  invisible()
}

# Unlike guards, ensures always execute following a matching guard, so no
# sophisticated logic is needed here.
.applyEnsure <- function(ensures, child, validate.fn, result, ...)
{
  if (is.null(ensures)) return(0)

  args <- list(...)
  idx <- 0
  valid <- TRUE
  f <- child
  for (g in ensures[[child]])
  {
    idx <- idx + 1
    valid <- valid && validate.fn(g,f, result, ...)

    if (! valid) break
  }
  ifelse(valid, 0, idx)
}

# This only works for one level
ensures <- function(fn, inherits=TRUE, child=NULL)
{
  if (! is.function(fn))
    stop("Ensure introspection can only be applied to functions")

  gfs <- attr(fn, 'ensure.fns', exact=TRUE)
  gxs <- attr(fn, 'ensure.xps', exact=TRUE)
  gs <- list(functions=gfs, expressions=gxs)

  if (! is.null(gfs) || ! is.null(gxs)) return(gs)
  if (! inherits) return(gs)

  parent <- sub('\\.[^.]+$','', deparse(substitute(fn)) )

  # At root of hierarchy
  if (length(grep('^get\\(', parent)) > 0) return(NULL)

  ensures(get(parent, inherits=TRUE), child=child)
}


.validateEnsureFunction <- function(g,f, result, ...)
{
  if (! is.function(g)) stop("Invalid ensure function specified")
  g(result, ...)
}

.validateEnsureExpression <- function(g,f, result, ...)
{
  f.exec <- get(f)
  my.guard <- NULL
  my.args <- paste(names(formals(f.exec)), collapse=',')
  xps <- parse(text=sprintf("my.guard <- function(result,%s) { %s }", my.args, g))
  eval(xps)
  my.guard(result, ...)
}

