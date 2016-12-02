context(' * testing token')

test_that('initialize', {
  coin <- token('50p', 30)
  expect_s3_class(coin, 'token')
  expect_true(is.token(coin))
  expect_equal(coin$value, '50p')
  expect_equal(coin$type, 30)
})
