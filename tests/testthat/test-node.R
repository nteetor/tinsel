context(' * testing node')

test_that('constructor', {
  symple <- token('syrup', -1, 1)
  nymble <- node(symple)
  expect_s3_class(nymble, 'node')
  expect_true(is.node(nymble))

  expect_has_fields(nymble, 'token', 'children')
  expect_has_methods(nymble, 'add')

  expect_equal(nymble$children, stack())
  expect_equal(value(nymble$token), 'syrup')
  expect_equal(type(nymble$token), -1)
  expect_equal(nymble$token$lineno, 1)
})

test_that('add method', {
  knot <- node(token('tree', 3, 1))
  leaf <- node(token('maple', 9, 1))
  knot$add(token('branch', 7, 1))$add(leaf)
  expect_equal(knot$children$size(), 2)
  expect_equal(vapply(as.list(knot$children), class, character(1)), c('node', 'node'))
  tokens_of <- lapply(as.list(knot$children), `[[`, 'token')
  expect_equal(value(tokens_of), c('maple', 'branch'))
  expect_equal(type(tokens_of), c(9, 7))
})
