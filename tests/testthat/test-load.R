context('testing loading')

`%is%` <- function(x, predicate) predicate(x)

expect_exists <- function(x) {
  e <- parent.frame()
  defined <- function(v) exists(v, envir = e, inherits = FALSE)

  eval(bquote(expect_true(.(x) %is% defined)))
}

test_that('error `file` must be character or connection', {
  expect_error(source_decoratees(3030), '`file` must be character or a connection')
})

test_that('error `file` path does not exist', {
  expect_error(source_decoratees('path/to/nowhere.R'), 'path specified by `file`')
})

test_that('error when missing definitions', {
  expect_error(source_decoratees('../testfiles/missing-definitions.R'), 'timer')
  expect_error(source_decoratees('../testfiles/bad-definition.R'), 'problem sourcing')
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

test_that('decorated function reference sourced', {
  source_decoratees('../testfiles/simple-functions.R')
  expect_exists('mean_inf')
  expect_is(mean_inf, 'decorated')
  expect_equal(mean_inf('100'), Inf)
  expect_equal(mean_inf(c(25, 50, 100)), mean(c(25, 50, 100)))
  expect_false(exists('bare_variable'))
})

test_that('multi-line function declaration sourced', {
  source_decoratees('../testfiles/simple-functions.R')
  expect_exists('one_fish')
  expect_is(one_fish, 'decorated')
  expect_equal(one_fish(), 'red fish, blue fish')
})
