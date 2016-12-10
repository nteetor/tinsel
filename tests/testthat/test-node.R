context(' * testing node')

test_that('node default constructor', {
  symple <- token('syrup', -1, 1)
  nymble <- node(symple)
  expect_s3_class(nymble, 'node')
  expect_true(is.node(nymble))

  expect_fields(nymble, 'token', 'children')
  expect_methods(nymble, 'add')

  expect_equal(nymble$children, stack())
  expect_equal(contents(nymble$token), 'syrup')
  expect_equal(type(nymble$token), -1)
  expect_equal(nymble$token$lineno, 1)
})

test_that('add internal method', {
  knot <- node(token('tree', 3, 1))
  leaf <- node(token('maple', 9, 1))
  knot$add(token('branch', 7, 1))
  knot$add(leaf)

  expect_equal(knot$children$size(), 2)
  expect_equal(vapply(as.list(knot$children), class, character(1)), c('node', 'node'))

  tokens_of <- lapply(as.list(knot$children), `[[`, 'token')
  expect_equal(contents(tokens_of), c('maple', 'branch'))
  expect_equal(type(tokens_of), c(9, 7))

  expect_error(knot$add(numeric(1)), 'cannot add numeric as child')
  expect_error(knot$add(data.frame()), 'cannot add data.frame as child')
})

test_that('type S3 method', {
  leaf <- node(token('maple', 9, 1))

  expect_equal(type(leaf), 9)
  expect_equal(type(leaf$add(token('new', 3, 1))), c(9, 3))
})

test_that('contents S3 method', {
  leaf <- node(token('maple', 9, 1))

  expect_equal(contents(leaf), 'maple')
  expect_equal(contents(leaf$add(token('syrup', 1, 1))), c('maple', 'syrup'))
})
