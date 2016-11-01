context('parsing file')

test_that('error when missing definition', {
  e <- new.env(parent = baseenv())
  expect_error(presents('../testfiles/missing-definitions.R', envir = e), 'timer')
})

test_that('non-decorated functions skipped', {
  e <- new.env(parent = baseenv())
  presents('../testfiles/sample-functions.R', envir = e)
  expect_true(!exists('g', envir = e, inherits = FALSE))
})

test_that('decorated functions loaded', {
  e <- new.env(parent = baseenv())
  presents('../testfiles/sample-functions.R', envir = e)
  expect_true(exists('f', envir = e, inherits = FALSE))
  expect_true(exists('h', envir = e, inherits = FALSE))
})
