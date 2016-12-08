context(' * testing token')

test_that('initialize', {
  coin <- token('50p', 30, 1)
  expect_s3_class(coin, 'token')
  expect_true(is.token(coin))
  expect_equal(coin$value, '50p')
  expect_equal(coin$type, 30)
  expect_equal(coin$lineno, 1)
})

test_that('as.character and print S3 generics', {
  coin <- token('50p', 99, 1)
  expect_equal(as.character(coin), '("50p", UNKNOWN)')
  expect_output(print(coin), '("50p", UNKNOWN)')

  bit <- token('doge', 101, 1)
  expect_equal(as.character(bit), '("doge", 101)')
})

test_that('field function', {
  arcade <- token('ten', 10, 1)
  ferry <- token('ship', 1642, 1)

  expect_null(field(list()))

  expect_error(type('ten'), 'unexpected class character')
  expect_error(c(ferry, 'S.S. Anne'), 'cannot combine token with character')
})

test_that('field<- function', {
  arcade <- token('ten', 10, 1)
  ferry <- token('ship', 1642, 1)

  expect_error(`field<-`(list(), 'deltron', 3030), 'unexpected class list')
})

test_that('type function', {
  arcade <- token('ten', 10, 1)
  ferry <- token('ship', 1642, 1)

  expect_equal(type(arcade), 10)
  expect_equal(type(c(arcade, ferry)), c(10, 1642))
  expect_equal(type(list(arcade)), 10)
})

test_that('type<- function', {
  knights <- token('scarf', 1066, 1)

  expect_silent(type(knights) <- .type$UNKNOWN)
  expect_equal(type(knights), .type$UNKNOWN)

  expect_error(type(knights) <- .type$DECORATER, 'token type may not be NULL')
})

test_that('value function', {
  arcade <- token('ten', 10, 1)
  ferry <- token('ship', 1642, 1)

  expect_equal(value(arcade), 'ten')
  expect_equal(value(list(arcade)), 'ten')
  expect_equal(value(c(arcade, ferry)), c('ten', 'ship'))
  expect_equal(value(c(ferry, c(arcade, ferry))), c('ship', 'ten', 'ship'))
})

test_that('value<- function', {
  knights <- token('scarf', 1066, 1)

  expect_silent(value(knights) <- 'glove')
  expect_equal(value(knights), 'glove')

  expect_error(value(knights) <- NULL, 'token value may not be NULL')
})
