library(futile.logger)
configLogger(threshold=DEBUG)
lg <- getLogger()


# Adds guards to the base function for functional dispatching
guard <- function(child.fn, value)
{
  #lg(DEBUG, sprintf('data = %s', data))
  child <- deparse(substitute(child.fn))
  lg(DEBUG, sprintf('Original function name: %s', child))
  parent <- sub('\\.[^.]+$','', child)
  lg(DEBUG, sprintf('Parent function name: %s', parent))

  if (! exists(parent))
    stop(sprintf("Function %s has no valid parent function", child))
  fn <- get(parent)
  gs <- attr(fn, 'guards')
  if (is.null(gs)) gs <- list()
  gs[[child]] <- c(gs[[child]], value)
  attr(fn, 'guards') <- gs
  assign(parent, fn, inherits=TRUE)

  invisible()
}

# Get the guards for a function. The function can be either the parent or any
# of its attached children
guards <- function(fn, inherits=TRUE)
{
  if (! is.function(fn))
    stop("Guard introspection can only be applied to functions")

  gs <- attr(fn, 'guards', exact=TRUE)
  if (! is.null(gs)) return(gs)
  if (! inherits) return(gs)

  parent <- sub('\\.[^.]+$','', deparse(substitute(fn)))
  guards(get(parent, inherits=TRUE), inherits=TRUE)
}



# Dispatcher for a more functional paradigm. This executes a function based on
# which guards are matched. The order of evaluation is based on the order the
# guards are declared.
UseFunction <- function(fn.name, ...)
{
  fn <- get(fn.name)
  gs <- guards(fn, inherits=FALSE)
  if (is.null(gs)) stop("Function must have guards for functional dispatching")

  args <- list(...)
  for (f in names(gs))
  {
    f.exec <- get(f)
    if (is.null(f.exec)) next
    if (length(formals(f.exec)) != length(args)) next

    valid <- TRUE
    for (g in gs[[f]])
    {
      if (is.logical(g)) valid <- valid && g
      else if (is.function(g)) valid <- valid && g(...)
      else 
      {
        msg <- "Skipping invalid guard '%s' for function '%s'"
        lg(WARN, sprintf(msg, g, f))
      }
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



