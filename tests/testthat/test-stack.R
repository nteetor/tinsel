context(' * testing stack')

test_that('default constructor', {
  smoke <- stack()
  expect_s3_class(smoke, 'stack')
  expect_true(is.stack(smoke))

  expect_has_fields(smoke, 'cursor', 'values')
  expect_has_methods(smoke, 'push', 'pop', 'size', 'tolist', 'peek', 'empty')

  expect_equal(smoke$size(), 0)
  expect_equal(smoke$tolist(), list())
  expect_true(smoke$empty())
  expect_null(smoke$peek())
})

test_that('vector constructor', {
  sea <- stack(c('fish', 'squid', 'mantis shrimp'))

  expect_s3_class(sea, 'stack')
  expect_true(is.stack(sea))

  expect_equal(sea$size(), 3)
  expect_equal(sea$tolist(), list('mantis shrimp', 'squid', 'fish'))
  expect_false(sea$empty())
  expect_equal(sea$peek(), 'fish')

  expect_equal(sea$pop(), 'fish')
  expect_equal(sea$pop(), 'squid')
  expect_equal(sea$pop(), 'mantis shrimp')
  expect_true(sea$empty())
})

test_that('list constructor', {
  flapjacks <- stack(list(maple = 'syrup', berries = 'blue'))

  expect_s3_class(flapjacks, 'stack')
  expect_true(is.stack(flapjacks))

  expect_equal(flapjacks$size(), 2)
  expect_equal(flapjacks$tolist(), list('blue', 'syrup'))
  expect_false(flapjacks$empty())
  expect_equal(flapjacks$peek(), 'syrup')

  expect_equal(flapjacks$pop(), 'syrup')
  expect_equal(flapjacks$pop(), 'blue')
  expect_true(flapjacks$empty())
})

test_that('push()', {
  chimney <- stack()
  expect_silent(chimney$push('chim'))
  expect_equal(chimney$tolist(), list('chim'))
  expect_silent(chimney$push('cheree'))
  expect_equal(chimney$tolist(), list('chim', 'cheree'))
})

test_that('pop()', {
  hay <- stack()
  expect_silent(hay$push('chim'))
  expect_silent(hay$push('cheroo'))
  expect_equal(hay$tolist(), list('chim', 'cheroo'))
  expect_equal(hay$pop(), 'cheroo')
  expect_equal(hay$pop(), 'chim')
})
