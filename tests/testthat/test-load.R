context('testing sourcing')

`%is%` <- function(x, predicate) predicate(x)

expect_exists <- function(x) {
  e <- parent.frame()
  defined <- function(x) exists(x, envir = e, inherits = FALSE)

  eval(bquote(expect_true(.(x) %is% defined)))
}

test_that('error when missing definition', {
  expect_error(source_decoratees('../testfiles/missing-definitions.R'), 'timer')
})

test_that('non-decorated functions skipped', {
  source_decoratees('../testfiles/simple-functions.R')
  expect_true(!exists('g', inherits = FALSE))
})

test_that('decoratees loaded', {
  source_decoratees('../testfiles/simple-functions.R')
  expect_exists('f')
  expect_is(f, 'function')
  expect_exists('h')
  expect_is(h, 'function')
})

test_that('pass arguments to decoratees', {
  expect_silent(source_decoratees('../testfiles/have-args.R'))
  expect_true(exists('my_name'))
})
