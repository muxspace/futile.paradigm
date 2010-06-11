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

guard(absdiff.21, 
  function(a,b) ! is.numeric(a) || ! is.numeric(b)
)
absdiff.21 <<- function(a, b) absdiff(as.double(a), as.double(b))

guard(absdiff.3, function(a) a > 5)
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

test.absdiff.3 <- function()
{
  checkTrue(12 == absdiff('6', 18))
  checkTrue(12 == absdiff(6,'18'))
  checkTrue(12 == absdiff('6','18'))
}

test.absdiff.4 <- function()
{
  checkTrue(12 == absdiff(6))
}
