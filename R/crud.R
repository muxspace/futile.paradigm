# Interface for create functions
# Example
#   create(my.class, arg1, arg2)
create <- function(x, ...)
{
  type <- deparse(substitute(x))
  type <- gsub('"','', type)
  err.obj <- NULL
  tryCatch(is.same <- type == x, error=function(e) err.obj <<- e)

  # ! is.null(err.obj) => use type (class symbol provided)
  # is.same => use type (string representation of class name)
  # else => use x (dynamically evaluated class name)
  if (is.null(err.obj) && ! is.same) type <- x

  # This is a special construction for create
  o <- AbuseMethod('create', type, type, ..., EXPLICIT=TRUE)
  class(o) <- c(type,class(o))
  o
}

# Convenience and safe-guard to ensure things don't blow up if not properly 
# defined in user code. An extra attribute is set to indicate that this is the
# default object.
create.default <- function(type, ...)
{
  o <- list(...)
  attr(o, 'isDefault') <- TRUE
  o
}

#access <- function(x, ...) AbuseMethod('access', x, ...)
#update <- function(x, ...) AbuseMethod('update', x, ...)
#delete <- function(x, ...) AbuseMethod('delete', x, ...)


