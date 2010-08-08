create.pear <<- function(klass, seeds=5, weight=10, type='bartlett')
{
  list(seeds=seeds, weight=weight, type=type)
}

test.create.default <- function()
{
  a <- create('apple', seeds=10)
  checkTrue(isa('apple',a))
  checkTrue(a$seeds == 10)
}

test.create.pear <- function()
{
  b <- create('pear', seeds=6)
  checkTrue(isa('pear',b))
  checkTrue(b$seeds == 6)
  checkTrue(b$weight == 10)
  checkTrue(b$type == 'bartlett')
}
