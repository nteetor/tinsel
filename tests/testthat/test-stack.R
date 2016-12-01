context(' * testing stack object')

test_that('default constructor', {
  smoke <- stack()
  expect_is(smoke, 'environment')
  expect_is(smoke$push, 'function')
  expect_is(smoke$pop, 'function')
  expect_is(smoke$size, 'function')
  expect_is(smoke$tolist, 'function')

  expect_equal(smoke$size(), 0)
  expect_equal(smoke$tolist(), list())
})

test_that('constructor', {
  flapjacks <- stack(list(maple = 'syrup', berries = 'blue'))
  expect_is(flapjacks, 'environment')
  expect_is(flapjacks$push, 'function')
  expect_is(flapjacks$pop, 'function')
  expect_is(flapjacks$size, 'function')
  expect_is(flapjacks$tolist, 'function')

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
