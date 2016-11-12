context('testing utils')

test_that('char_at', {
  expect_error(char_at('error', 0), 'index out of bounds')
  expect_error(char_at('ire', 10))

  expect_equal('a', char_at('abc', 1))
  expect_equal('c', char_at('abc', -1))
})

test_that('first_of', {
  expect_equal(first_of('food', 'd'), 4)
  expect_equal(first_of('dough', 'd'), 1)
})
