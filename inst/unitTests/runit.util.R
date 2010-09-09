o <<- create(Dummy, a=3, c=5)

test.hasa.single <- function()
{
  checkTrue(hasa(a, o))
  checkTrue(hasa(c, o))
  checkTrue(! hasa(b, o))
}

test.hasa.multiple <- function()
{
  checkTrue(all(c(TRUE,FALSE) == hasa(c(a,b), o)) )
  checkTrue(all(c(TRUE,TRUE) == hasa(c(a,c), o)) )
}

test.hasall.single <- function()
{
  checkTrue(hasall(a, o))
  checkTrue(hasall(c, o))
  checkTrue(! hasall(b, o))
}

test.hasall.multiple <- function()
{
  checkTrue(! hasall(c(a,b), o))
  checkTrue(hasall(c(a,c), o))
}

