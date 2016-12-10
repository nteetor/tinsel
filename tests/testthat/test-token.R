context(' * testing token')

test_that('token constructor', {
  coin <- token('50p', 30, 1)

  expect_s3_class(coin, 'token')
  expect_true(is.token(coin))
  expect_equal(coin$contents, '50p')
  expect_equal(coin$type, 30)
  expect_equal(coin$lineno, 1)
})

test_that('as.character and print S3 generics', {
  coin <- token('50p', 99, 1)

  expect_equal(as.character(coin), '("50p" UNKNOWN)')
  expect_output(print(coin), '("50p" UNKNOWN)')

  bit <- token('doge', 101, 1)

  expect_equal(as.character(bit), '("doge" 101)')
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

test_that('contents function', {
  arcade <- token('ten', 10, 1)
  ferry <- token('ship', 1642, 1)

  expect_equal(contents(arcade), 'ten')
  expect_equal(contents(list(arcade)), 'ten')
  expect_equal(contents(c(arcade, ferry)), c('ten', 'ship'))
  expect_equal(contents(c(ferry, c(arcade, ferry))), c('ship', 'ten', 'ship'))
})

test_that('contents<- function', {
  knights <- token('scarf', 1066, 1)

  expect_silent(contents(knights) <- 'glove')
  expect_equal(contents(knights), 'glove')

  expect_error(contents(knights) <- NULL, 'token value must be of class character')
})
