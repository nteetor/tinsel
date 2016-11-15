context('helper functions')

test_that('is.decorated function', {
  source_decoratees('../testfiles/character-defaults.R')
  goodbye <- function() 'goodbye moon'
  expect_true(exists('hello'))
  expect_true(is.decorated(hello))
  expect_false(is.decorated(goodbye))
})

test_that('print.decorated function', {
  source_decoratees('../testfiles/simple-functions.R')
  expect_output(print(fib), 'Time elapsed')
  expect_output(print(progress), 'fval')
})

test_that('decorated function', {
  source_decoratees('../testfiles/simple-functions.R')
  expect_true(exists('progress'))
  expect_is(decorators(progress), 'list')
  expect_equal(length(decorators(progress)), 2)
  expect_equal(names(decorators(progress)), c('per_centum', 'timer'))
  expect_error(decorators(1))
  expect_error(decorators(mean))
})

test_that('original function', {
  source_decoratees('../testfiles/simple-functions.R')
  expect_true(exists('fib'))
  lie <- function(n) {
    round(((1.61805 ** (1:n - 1)) + (1.61805 ** (1:n - 2))) / sqrt(5))
  }
  expect_is(original(fib), 'function')
  expect_equal(body(original(fib)), body(lie))
  expect_error(original('F-Zero'))
  expect_error(original(lm))
})
