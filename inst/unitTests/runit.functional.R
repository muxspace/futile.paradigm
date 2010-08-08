# For some reason these variables have to be declared globally, otherwise the
# scoping within RUnit breaks the tests.
absdiff <<- function(...) UseFunction('absdiff', ...)

guard(absdiff.1, c(
  function(a,b) is.numeric(a) && is.numeric(b),
  function(a,b) a > b
))
absdiff.1 <<- function(a, b) a - b

guard(absdiff.2, c(
  function(a,b) is.numeric(a) && is.numeric(b),
  function(a,b) a < b
))
absdiff.2 <<- function(a, b) b - a

guard(absdiff.21, ! is.numeric(a) || ! is.numeric(b))
absdiff.21 <<- function(a, b) absdiff(as.double(a), as.double(b))

# This converts to integers. This will be called if c and 
# optionally d is named
guard(absdiff.int, TRUE)
absdiff.int <<- function(c, d) absdiff(as.integer(c), as.integer(d))

# This uses log values. Note that this will only be executed if
# l is explicitly named (and all preceding 2 argument functions
# are strict).
guard(absdiff.log, TRUE)
absdiff.log <<- function(a, l) absdiff(log(a), log(l))

guard(absdiff.3, a > 5)
absdiff.3 <<- function(a) a * 2

guard(absdiff.4, TRUE)
absdiff.4 <<- function(a) a + 1

## TESTS
test.absdiff.1 <- function()
{
  checkTrue(12 == absdiff(18,6))
}

test.absdiff.2 <- function()
{
  checkTrue(12 == absdiff(6,18))
}

test.absdiff.21 <- function()
{
  checkTrue(12 == absdiff('6', 18))
  checkTrue(12 == absdiff(6,'18'))
  checkTrue(12 == absdiff('6','18'))
}

test.absdiff.3 <- function()
{
  checkTrue(12 == absdiff(6))
}

test.absdiff.4 <- function()
{
  checkTrue(6 == absdiff(5))
  checkTrue(0 == absdiff(-1))
}

test.absdiff.int <- function()
{
  checkTrue(12 == absdiff(c=3.5, 15.1))
  checkTrue(12 == absdiff(c=3.5, d=15.1))
  checkTrue(12 == absdiff(d=3.5, c=15.1))
}

test.absdiff.int.x <- function()
{
  cat("Expecting exception\n")
  checkException(12 == absdiff(c=3.5, a=15.1))
}

test.absdiff.log <- function()
{
  checkTrue(1.461932 - absdiff(3.5, l=15.1) < 0.000001)
  checkTrue(1.461932 - absdiff(l=3.5, 15.1) < 0.000001)
}


## DEFINITIONS FOR ADVANCED GUARDS (INCOMPLETE)
interpolate <<- function(...) UseFunction('interpolate',...)

guard(interpolate, isa('linear', cfg))
interpolate <<- function(cfg, a,b) { }

