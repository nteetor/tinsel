context(' * testing token')

test_that('initialize', {
  coin <- token('50p', 30)
  expect_s3_class(coin, 'token')
  expect_true(is.token(coin))
  expect_equal(coin$value, '50p')
  expect_equal(coin$type, 30)
})

test_that('accessor functions', {
  arcade <- token('ten', 10)
  expect_equal(type(arcade), 10)
  expect_equal(value(arcade), 'ten')

  expect_error(type('ten'), 'unexpected class character')
  expect_error(value(10), 'unexpected class numeric')
})
