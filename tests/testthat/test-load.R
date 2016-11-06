context('parsing file')

test_that('error when missing definition', {
  expect_error(source_decorated('../testfiles/missing-definitions.R'), 'timer')
})

test_that('non-decorated functions skipped', {
  source_decorated('../testfiles/sample-functions.R')
  expect_true(!exists('g', inherits = FALSE))
})

test_that('decorated functions loaded', {
  source_decorated('../testfiles/sample-functions.R')
  expect_true(exists('f', inherits = FALSE))
  expect_true(exists('h', inherits = FALSE))
})
