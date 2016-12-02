context(' * testing stack')

test_that('default constructor', {
  smoke <- stack()
  expect_s3_class(smoke, 'stack')
  expect_true(is.stack(smoke))

  expect_has_fields(smoke, 'cursor', 'values')
  expect_has_methods(smoke, 'push', 'pop', 'size', 'tolist')

  expect_equal(smoke$size(), 0)
  expect_equal(smoke$tolist(), list())
})

test_that('constructor', {
  flapjacks <- stack(list(maple = 'syrup', berries = 'blue'))

  expect_s3_class(flapjacks, 'stack')
  expect_true(is.stack(flapjacks))

  expect_has_fields(flapjacks, 'cursor', 'values')
  expect_has_methods(flapjacks, 'push', 'pop', 'size', 'tolist')

  expect_equal(flapjacks$size(), 2)
  expect_equal(flapjacks$tolist(), list('syrup', 'blue'))
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
