context(' * testing custom errors')

test_that('condition function', {
  simple <- condition('simple', 'just a simple error')

  expect_s3_class(simple, c('simple', 'error', 'condition'))
  expect_equal(simple$message, 'just a simple error')

  out_of_bounds <- condition('bounds', 'out of bounds', line = 1, object = 'name')

  expect_s3_class(out_of_bounds, c('bounds', 'error', 'condition'))
  expect_equal(out_of_bounds$message, 'out of bounds')
  expect_attribute(out_of_bounds, 'line', 1)
  expect_attribute(out_of_bounds, 'object', 'name')
})

test_that('expected function', {
  expt <- expected('E', '3', 7)
  expect_s3_class(expt, c('expected', 'error', 'condition'))
  expect_equal(expt$message, 'found "3" on line 7, expected "E"')
})

test_that('elaborate function', {
  expnum <- expected(.sym$NUMBER, 'A', 1)
  expect_error(stop(expnum), 'found "A" on line 1, expected a numeric character')
  explet <- expected(.sym$LETTER, 0, 1)
  expect_error(stop(explet), 'found "0" on line 1, expected an alphabetic character')
  expfname <- expected(.sym$FILENAME_CHAR, '\\', 1)
  expect_error(stop(expfname), 'found "\\" on line 1, expected a character allowed in a file name', fixed = TRUE)
  expsyn <- expected(.sym$SYNTACTIC_CHAR, '%', 1)
  expect_error(stop(expsyn), 'found "%" on line 1, expected an alphanumeric character, "_", or "."', fixed = TRUE)

  expect_equal(elaborate(.type$TINSEL_COMMENT), 'a tinsel comment')
  expect_equal(elaborate(.type$FILE_REFERENCE), 'FILE_REFERENCE')

  expect_error(elaborate(Inf), "cannot elaborate upon Inf, you're on your own")
})


