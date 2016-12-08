context(' * testing clone')

test_that('aget function', {
  e <- new.env(parent = emptyenv())
  e$fizz <- 'whizz'
  e$howdy <- 'doody'
  e$ten <- 10
  e$f <- new.env(parent = emptyenv())

  expect_equal(aget(e), list(fizz = 'whizz', howdy = 'doody', f = e$f, ten = 10))
  expect_equal(aget(e, mode = 'environment'), list(f = e$f))
  expect_equal(aget(e, mode = 'logical'), list())
  expect_equal(aget(emptyenv()), list())
})

test_that('clone function', {
  e <- new.env(parent = emptyenv())
  class(e) <- 'eclass'
  e$fizz <- 'whizz'
  e$howdy <- 'doody'
  e$ten <- 10

  # shallow clone
  expect_equal(class(clone(e$ten)), class(clone(e$ten)))
  expect_equal(e$ten, clone(e$ten))
  expect_equal(class(e), class(clone(e)))
  expect_equal(e, clone(e))

  # deep clone
  e$f <- new.env(parent = emptyenv())
  e$f$hello <- 'world'
  ln <- clone(e)

  expect_equal(class(e), class(ln))
  expect_equal(e$ten, ln$ten)
  expect_equal(e$f, ln$f)

  e$f$hello <- 'moon'
  expect_equal(ln$f$hello, 'world')

  rm('e', inherits = FALSE)
  expect_equal(ln$fizz, 'whizz')
  expect_equal(ln$f$hello, 'world')
})
