#library(futile.logger)
#configLogger(threshold=DEBUG)
#lg <- getLogger()
paradigm.options <- OptionsManager('paradigm.options')

# Registers function environments, which is necessary for proper handling
# within packages that depend on futile.paradigm.
register <- function(fn.name, where)
{
  paradigm.options(update=list(fn.name, where))
  invisible()
}

# Adds guards to the base function for functional dispatching
guard <- function(child.fn, condition, strict=TRUE)
{
  child <- deparse(substitute(child.fn))

  expr <- deparse(substitute(condition))
  if (length(grep('function', expr)) < 1) 
    return(.guard.expression(child, expr, strict))

  return(.guard.function(child, condition, strict))
}

# Shortcut form for simple pattern matches
.guard.function <- function(child, condition, strict)
{
  parent <- sub('\\.[^.]+$','', child)
  where <- paradigm.options(parent)
  if (is.null(where)) where <- -1

  if (! exists(parent, where))
  {
    msg <- "Function %s has no visible parent function '%s'"
    stop(sprintf(msg, child, parent))
  }
  fn <- get(parent, where)
  gs <- attr(fn, 'guard.fns')
  if (is.null(gs)) gs <- list()
  gs[[child]] <- c(gs[[child]], condition)
  attr(fn, 'guard.fns') <- gs

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

# Shortcut form for expressions instead of more verbose functions
# This is the standard way of writing guards, although the long form is slightly
# more efficient from an execution perspective
.guard.expression <- function(child, expr, strict)
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
  gs <- attr(fn, 'guard.xps')
  if (is.null(gs)) gs <- list()
  gs[[child]] <- c(gs[[child]], expr)
  attr(fn, 'guard.xps') <- gs

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

# Get the guards for a function. The function can be either the parent or any
# of its attached children
guards <- function(fn, inherits=TRUE)
{
  if (! is.function(fn))
    stop("Guard introspection can only be applied to functions")

  gfs <- attr(fn, 'guard.fns', exact=TRUE)
  gxs <- attr(fn, 'guard.xps', exact=TRUE)
  gs <- list(functions=gfs, expressions=gxs)
  if (! is.null(gfs) || ! is.null(gxs)) return(gs)
  if (! inherits) return(gs)

  parent <- sub('\\.[^.]+$','', deparse(substitute(fn)))
  guards(get(parent, inherits=TRUE), inherits=TRUE)
}

# Operates on a child function or function name
isStrict <- function(child.fn)
{
  if (is.function(child.fn)) child <- deparse(substitute(child.fn))
  else child <- child.fn

  parent <- sub('\\.[^.]+$','', child)
  fn <- get(parent, inherits=TRUE)

  ss <- attr(fn, 'strict', exact=TRUE)
  #cat("Got strict on function:",ss[[child]],"\n")
  if (! is.null(ss) && ! is.null(ss[[child]])) return(ss[[child]])

  FALSE
}

isa <- function(type, argument)
{
  type <- gsub('[\'"]','',deparse(substitute(type)))
  type %in% class(argument)
}


# Dispatcher for a more functional paradigm. This executes a function based on
# which guards are matched. The order of evaluation is based on the order the
# guards are declared.
UseFunction <- function(fn.name, ...)
{
  fn <- get(fn.name)
  gs <- guards(fn, inherits=FALSE)
  if (is.null(gs)) stop("Incorrect guard output. Please report to maintainer.")
  if (is.null(gs$functions) && is.null(gs$expressions))
    stop("Function must have guards for functional dispatching")

  args <- list(...)
  # First iterate through formal functions. Formal functions have precedence
  # over expressions.
  for (f in names(gs$functions))
  {
    # First check that this function is valid and matches the argument count
    f.exec <- get(f)
    if (is.null(f.exec)) next
    if (length(formals(f.exec)) != length(args)) next

    # If strict, match exactly the function arguments with the arguments
    # passed in. This is the default behavior.
    non.empty <- names(args)[nchar(names(args)) > 0]
    if (length(non.empty) > 0 && isStrict(f) && 
        length(setdiff(non.empty, names(formals(f)))) > 0 )
      next

    valid <- TRUE
    for (g in gs$functions[[f]])
    {
      if (is.logical(g)) valid <- valid && g
      else if (is.function(g)) valid <- valid && g(...)
      else 
      {
        msg <- "Skipping invalid guard '%s' for function '%s'"
        cat(sprintf(msg, g, f))
        next
      }
      # NOTE: If later on we want guard sequences instead, test for valid
      # instead so the logic becomes a disjunction
      if (! valid) break
    }
    if (valid) return(do.call(f, list(...)))
  }

  # Now loop through guard expressions
  for (f in names(gs$expressions))
  {
    # First check that this function is valid and matches the argument count
    f.exec <- get(f)
    if (is.null(f.exec)) next
    if (length(formals(f.exec)) != length(args)) next

    # If strict, match exactly the function arguments with the arguments
    # passed in. This is the default behavior.
    non.empty <- names(args)[nchar(names(args)) > 0]
    if (length(non.empty) > 0 && isStrict(f) && 
        length(setdiff(non.empty, names(formals(f)))) > 0 )
      next

    valid <- TRUE
    # Construct functions based on the function definition
    for (g in gs$expressions[[f]])
    {
      my.guard <- NULL
      my.args <- paste(names(formals(f.exec)), collapse=',')
      xps <- parse(text=sprintf("my.guard <- function(%s) { %s }", my.args, g))
      eval(xps)
      valid <- valid && my.guard(...)
      if (! valid) break
    }
    if (valid) return(do.call(f, list(...)))
  }
  stop("No valid function for arguments")
}

# General framework for accessing an object. For create and access, strings can
# be used to identify the class type. These don't use UseMethod for syntactic
# sugar inconsistencies (UseMethod passes original value as opposed to 
# transformed values (i.e. from symbol to character).
# The idea for AbuseMethod is to define high-level framework APIs that can be 
# extended by user code. This produces some consistency in the way common
# operations are called. Functions dispatched from AbuseMethod can contain 
# UseFunction declarations for further dispatching.
AbuseMethod <- function(fn.name, type, ..., EXPLICIT=FALSE)
{
  if (EXPLICIT)
  {
    target <- paste(fn.name,type,sep='.')
    if (! exists(target)) target <- paste(fn.name,'default',sep='.')

    do.call(target, list(...) )
  }
  else
  {
    types <- class(type)
    for (t in types)
    {
      target <- paste(fn.name,t,sep='.')
      if (exists(target)) break
    }
    if (! exists(target)) target <- paste(fn.name,'default',sep='.')
    do.call(target, c(type, list(...)) )
  }
}



