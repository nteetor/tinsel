context('testing sourcing')

`%is%` <- function(x, predicate) predicate(x)

expect_exists <- function(x) {
  e <- parent.frame()
  defined <- function(v) exists(v, envir = e, inherits = FALSE)

  eval(bquote(expect_true(.(x) %is% defined)))
}

test_that('arguments correct class / exist', {
  expect_error(source_decoratees(3030), '`file` must be character or a connection')
  expect_error(source_decoratees('path/to/nowhere.R'), 'path specified by `file`')
  expect_error(source_decoratees('../testfiles/simple-functions.R', into = 1),
               'argument `into` must be environment')
  expect_error(source_decoratees('../testfiles/simple-functions.R', into = 'fizz'),
               'argument `into` must be environment')
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

test_that('decorators from separate files sourced', {
  source_decoratees('../testfiles/includes-files.R')
  expect_exists('div_scale')
  expect_s3_class(div_scale, c('decorator', 'function'))
  expect_exists('dbl_c')
  expect_s3_class(dbl_c, c('decorator', 'function'))
  expect_equal(dbl_c(1:5), as.double(1:5))
  expect_equal(dbl_c(1, dbl_c(5:8), '40'), as.double(c(1, 5, 6, 7, 8, 40)))
})

test_that('decorators separate file with extention', {
  source_decoratees('../testfiles/includes-files.R')
  expect_exists('boring')
  expect_s3_class(boring, c('decorator', 'function'))

})

test_that('error for incorrect decorator file names', {
  expect_error(source_decoratees('../testfiles/missing-separate.R'),
               'could not find decorator file')
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
