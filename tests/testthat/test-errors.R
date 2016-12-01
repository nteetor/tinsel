context(' * testing custom errors')

expect_attribute <- function(object, attribute, value) {
  eval(bquote(expect_equal(.(attr(object, attribute, TRUE)), .(value))))
}

test_that('condition constructor', {
  simple <- condition('simple', 'just a simple error')
  expect_s3_class(simple, c('simple', 'error', 'condition'))
  expect_equal(simple$message, 'just a simple error')

  out_of_bounds <- condition('bounds', 'out of bounds', line = 1, object = 'name')
  expect_s3_class(out_of_bounds, c('bounds', 'error', 'condition'))
  expect_equal(out_of_bounds$message, 'out of bounds')
  expect_attribute(out_of_bounds, 'line', 1)
  expect_attribute(out_of_bounds, 'object', 'name')
})

test_that('expected condition', {
  expt <- expected('E', '3', 7, extra = 'garlic')
  expect_s3_class(expt, c('expected', 'error', 'condition'))
  expect_equal(expt$message, 'found "3" on line 7, expected "E"')
  expect_attribute(expt, 'symbol', 'E')
  expect_attribute(expt, 'actual', '3')
  expect_attribute(expt, 'lineno', 7)
  expect_attribute(expt, 'extra', 'garlic')
})
