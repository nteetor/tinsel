context(' * testing utils')

test_that('%||%', {
  expect_equal(NULL %||% 3, 3)
  expect_equal(NULL %||% 'hello', 'hello')
  expect_equal(list(1, 2) %||% c(1, 2), list(1, 2))
  expect_null(NULL %||% NULL)
})

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

test_that('re_split', {
  expect_equal(re_split('fox', ''), c('f', 'o', 'x'))
  expect_equal(re_split('123,abc', ','), c('123', 'abc'))
  expect_equal(re_split('1a2b3c', '[a-z]'), c('1', '2', '3'))
  expect_equal(re_split('1a2b3c', '\\d'), c('', 'a', 'b', 'c'))
})

test_that('re_search', {
  string1 <- 'falcon'
  expect_equal(re_search(string1, '(\\w){3}$'), 'con')
  string2 <- 'captain'
  expect_null(re_search(string2, 'punch'))
})

test_that('re_match', {
  expect_true(re_match('helloworld', 'hello\\w+'))
  expect_true(re_match('simple', 'simple'))

  expect_false(re_match('hellomoon', 'hello'))
  expect_false(re_match('hellosun', 'sun'))
  expect_false(re_match('  _', '\\s'))
})

test_that('set_names', {
  lyst <- list(1, 2, 3)
  lyst <- set_names(lyst, c('one', 'two', 'three'))
  myst <- list(one = 1, two = 2, three = 3)

  expect_named(lyst, c('one', 'two', 'three'))
  expect_equal(set_names(lyst, c('one', 'two', 'three')), myst)
})

test_that('cat0', {
  expect_output(cat0('hello', 'world'), '^helloworld$')
  expect_output(cat0('hello   ', 'moon'), '^hello   moon$')
})
