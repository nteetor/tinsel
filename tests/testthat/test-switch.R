context(' * testing if_*')

test_that('is_blank_name()', {
  expect_true(is_blank_name(alist(x = )[[1]]))
  expect_false(is_blank_name(as.name('fizz')))
})

test_that('if_symbol()', {
  expect_error(if_symbol('c', C = 'sea'), 'unexpected name(s) C', fixed = TRUE)
  expect_error(if_symbol('d', EOF = -1, D = 'dee'), 'unexpected name(s) D', fixed = TRUE)

  expect_equal(if_symbol('1', PERIOD = 1, IDENTIFIER_CHAR = 2), NULL)
  expect_equal(if_symbol('1', NUMBER = 'one'), 'one')
  expect_equal(if_symbol('1', LPAREN = ':', NUMBER = 'one'), 'one')
  expect_equal(if_symbol('1', NUMBER = , LETTER = '\\w'), '\\w')
  expect_equal(if_symbol('e', EXPNOTATION = 'E', EOF = , EOL = -1), 'E')

  expect_error(if_symbol('e', EXPNOTATION = do_e), 'no definition found for do_e')
})

test_that('if_type()', {
  expect_equal(if_type(99, LOOKAHEAD = -1), -1)
  expect_equal(if_type(1, PROGRAM = function() 777), 777)
  lokal <- 'hello, world'
  focal <- function() paste0(lokal, '!')
  expect_equal(if_type(3, STATEMENT = focal), 'hello, world!')
})
