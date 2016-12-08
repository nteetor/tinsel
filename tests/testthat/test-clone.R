context(' * testing clone')

test_that('aget()', {
  e <- new.env(parent = emptyenv())
  e$fizz <- 'whizz'
  e$howdy <- 'doody'
  e$ten <- 10
  e$f <- new.env(parent = emptyenv())
  expect_equal(aget(e), list(fizz = 'whizz', howdy = 'doody', f = e$f, ten = 10))
  expect_equal(aget(e, mode = 'environment'), list(f = e$f))
})
