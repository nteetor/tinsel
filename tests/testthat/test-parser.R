context(' * testing parser')

test_that('parser constructor', {
  tinyser <- parser('../testfiles/tiny.R')

  expect_s3_class(tinyser, 'parser')
  expect_true(is.parser(tinyser))
  expect_fields(tinyser, 'scanner', 'tokens', 'lookup', 'parsed')
  expect_methods(tinyser, 'parse', 'shift', 'reduce', 'step')
})

test_that('parse internal method', {
  tinyser <- parser('../testfiles/tiny.R')

  nohds <- tinyser$parse()
  expect_equal(type(nohds), c(
    .type$SOF,
    .type$TINSEL_COMMENT,
    .type$DECORATEE,
    .type$IDENTIFIER,
    .type$DECORATOR,
    .type$CALL,
    .type$PACKAGE_ACCESSOR,
    .type$IDENTIFIER,
    .type$PACKAGE_NAME,
    .type$FILE_REFERENCE
  ))
})


