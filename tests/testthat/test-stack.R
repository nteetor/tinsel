context(' * testing stack')

test_that('stack default constructor', {
  smoke <- stack()

  expect_s3_class(smoke, 'stack')
  expect_true(is.stack(smoke))
  expect_fields(smoke, 'cursor', 'values')
  expect_methods(smoke, 'push', 'pop', 'size', 'peek', 'empty')
  expect_equal(smoke$size(), 0)
  expect_equal(as.list(smoke), list())
  expect_true(smoke$empty())
  expect_null(smoke$peek())
})

test_that('stack vector constructor', {
  sea <- stack(c('fish', 'squid', 'mantis shrimp'))

  expect_s3_class(sea, 'stack')
  expect_true(is.stack(sea))

  expect_equal(sea$size(), 3)
  expect_equal(as.list(sea), list('mantis shrimp', 'squid', 'fish'))
  expect_false(sea$empty())
  expect_equal(sea$peek(), 'mantis shrimp')

  expect_equal(sea$pop(), 'mantis shrimp')
  expect_equal(sea$pop(), 'squid')
  expect_equal(sea$pop(), 'fish')
  expect_true(sea$empty())
})

test_that('stack list constructor', {
  flapjacks <- stack(list(maple = 'syrup', berries = 'blue'))

  expect_s3_class(flapjacks, 'stack')
  expect_true(is.stack(flapjacks))

  expect_equal(flapjacks$size(), 2)
  expect_equal(as.list(flapjacks), list('blue', 'syrup'))
  expect_false(flapjacks$empty())
  expect_equal(flapjacks$peek(), 'blue')

  expect_equal(flapjacks$pop(), 'blue')
  expect_equal(flapjacks$pop(), 'syrup')
  expect_true(flapjacks$empty())
})

test_that('push internal method', {
  chimney <- stack()
  expect_silent(chimney$push('chim'))
  expect_equal(as.list(chimney), list('chim'))
  expect_silent(chimney$push('cheree'))
  expect_equal(as.list(chimney), rev(list('chim', 'cheree')))
})

test_that('pop function', {
  hay <- stack()
  hay$push('chim')
  hay$push('cheroo')

  expect_equal(as.list(hay), rev(list('chim', 'cheroo')))
  expect_equal(hay$size(), 2)
  expect_equal(hay$pop(), 'cheroo')
  expect_equal(hay$size(), 1)
  expect_equal(hay$pop(), 'chim')
})

test_that('as.list S3 generic', {
  aircraft <- stack()
  aircraft$push(3030)$push(4040)

  expect_equal(as.list(aircraft), list(4040, 3030))

  rm('1', envir = aircraft$values, inherits = FALSE)
  expect_error(as.list(aircraft), 'unexpected error coercing stack to list')
})

test_that('as.character and print S3 generic', {
  shack <- stack()
  shack$push(10)$push(20)

  expect_equal(as.character(shack), '[ {20}, {10} ]')
  expect_output(print(shack), '# A stack: 2 \n1\\s+20 <numeric>\n2\\s+10 <numeric>')
  expect_output(print(shack, n = 1), '# A stack: 2 \n1\\s+20 <numeric>\n# ... with 1 more items')

  expect_equal(as.character(stack()), '[ ]')
  expect_output(print(stack()), '# A stack: 0')
})
