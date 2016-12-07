context(' * testing token')

test_that('initialize', {
  coin <- token('50p', 30, 1)
  expect_s3_class(coin, 'token')
  expect_true(is.token(coin))
  expect_equal(coin$value, '50p')
  expect_equal(coin$type, 30)
  expect_equal(coin$lineno, 1)
})

test_that('accessor functions', {
  arcade <- token('ten', 10, 1)
  ferry <- token('ship', 1642, 1)
  expect_equal(type(arcade), 10)
  expect_equal(value(arcade), 'ten')
  expect_equal(type(c(arcade, ferry)), c(10, 1642))
  expect_equal(value(c(arcade, ferry)), c('ten', 'ship'))
  expect_equal(value(c(ferry, c(arcade, ferry))), c('ship', 'ten', 'ship'))

  expect_error(type('ten'), 'unexpected class character')
  expect_error(value(10), 'unexpected class numeric')
  expect_error(c(ferry, 'S.S. Anne'), 'cannot combine token with character')
})

test_that('setter functions', {
  knights <- token('scarf', 1066, 1)
  expect_equal(type(knights), 1066)
  expect_silent(type(knights) <- .type$UNKNOWN)
  expect_equal(type(knights), .type$UNKNOWN)

  expect_error(type(knights) <- 1098, 'unknown type 1098')
  expect_error(type(knights) <- .type$DECORATER, 'type cannot be NULL')
})
