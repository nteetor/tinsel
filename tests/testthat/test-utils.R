context('testing utils')

test_that('char_at', {
  expect_error(char_at('error', 0), 'index out of bounds')
  expect_error(char_at('ire', 10))

  expect_equal('a', char_at('abc', 1))
  expect_equal('c', char_at('abc', -1))
})

test_that('first_of', {
  expect_equal(first_of('abcdef', '1'), -1)
  expect_equal(first_of('food', 'd'), 4)
  expect_equal(first_of('dough', 'd'), 1)
})

test_that('re_search', {
  string1 <- 'falcon'
  expect_equal(re_search(string1, '(\\w){3}$'), 'con')
  string2 <- 'captain'
  expect_null(re_search(string2, 'punch'))
})

test_that('cat0', {
  expect_output(cat0('hello', 'world'), '^helloworld$')
  expect_output(cat0('hello   ', 'moon'), '^hello   moon$')
})
