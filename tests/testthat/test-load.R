context('testing sourcing')

`%is%` <- function(x, predicate) predicate(x)

expect_exists <- function(x) {
  e <- parent.frame()
  defined <- function(v) exists(v, envir = e, inherits = FALSE)

  eval(bquote(expect_true(.(x) %is% defined)))
}

test_that('error when missing definition', {
  expect_error(source_decoratees('../testfiles/missing-definitions.R'), 'timer')
})

test_that('non-decorated functions skipped', {
  source_decoratees('../testfiles/simple-functions.R')
  expect_true(!exists('ignore', inherits = FALSE))
})

test_that('decoratees loaded', {
  source_decoratees('../testfiles/simple-functions.R')
  expect_exists('fib')
  expect_is(fib, 'function')
  expect_is(fib, 'decorated')
  expect_exists('progress')
  expect_is(progress, 'function')
  expect_is(progress, 'decorated')
})

test_that('correct order of decorators', {
  source_decoratees('../testfiles/simple-functions.R')
  progress_decorators <- names(attr(progress, 'decorators', exact = TRUE))
  expect_equal(progress_decorators, c('per_centum', 'timer'))
})

test_that('pass arguments to decoratees', {
  expect_silent(source_decoratees('../testfiles/have-args.R'))
  expect_true(exists('my_name'))
})
