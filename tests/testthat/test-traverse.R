context('  * testing traversal')

`%has_method%` <- function(o, m) {
  exists(m, envir = o, inherits = FALSE)
}

expect_has_method <- function(class, method) {
  eval(bquote(expect_true(class %has_method% .(method))))
}

test_that('%has_method% helper', {
  twavel <- traverse('../testfiles/tongue-twister.txt')
  expect_false(emptyenv() %has_method% 'toString')
  expect_true(baseenv() %has_method% 'mean')
})

test_that('initialize traversal', {
  twavel <- traverse('../testfiles/tongue-twister.txt')
  expect_is(twavel, 'environment')
  expect_type(twavel$cursor, 'double')
  expect_equal(twavel$cursor, 1)
  expect_type(twavel$chars, 'character')
  expect_equal(length(twavel$chars), 42)

  expect_has_method(twavel, 'at_eof')
  expect_false(twavel$at_eof())
  expect_has_method(twavel, 'size')
  expect_equal(twavel$size(), 42)
  expect_has_method(twavel, 'increment_cursor')
  expect_has_method(twavel, 'decrement_cursor')
  expect_has_method(twavel, 'getchar')
  expect_has_method(twavel, 'getregex')
  expect_has_method(twavel, 'getline')
  expect_has_method(twavel, 'putchar')
  expect_has_method(twavel, 'peek')
  expect_has_method(twavel, 'lookfor')
  expect_has_method(twavel, 'skipws')
  expect_has_method(twavel, 'expect')
})

test_that('increment_cursor()', {
  twavel <- traverse('../testfiles/tongue-twister.txt')
  expect_equal(twavel$cursor, 1)
  twavel$increment_cursor()
  expect_equal(twavel$cursor, 2)
  twavel$increment_cursor()
  expect_equal(twavel$cursor, 3)
  for (i in seq_len(41)) twavel$increment_cursor()
  expect_equal(twavel$cursor, 43)
})

test_that('decrement_cursor()', {
  twavel <- traverse('../testfiles/tongue-twister.txt')
  expect_equal(twavel$cursor, 1)
  twavel$decrement_cursor()
  expect_equal(twavel$cursor, 1)
  twavel$increment_cursor()
  twavel$increment_cursor()
  twavel$decrement_cursor()
  expect_equal(twavel$cursor, 2)
})

test_that('getchar()', {
  twavel <- traverse('../testfiles/tongue-twister.txt')
  expect_equal(twavel$getchar(), 'T')
  expect_equal(twavel$getchar(), 'h')
  expect_equal(twavel$getchar(), 'e')
  expect_equal(twavel$getchar(), '\n')
  expect_equal(twavel$getchar(), 's')
  for (i in seq_len(twavel$size())) twavel$getchar()
  expect_equal(twavel$getchar(), 'EOF')
})

test_that('getregex()', {
  twavel <- traverse('../testfiles/tongue-twister.txt')
  expect_equal(twavel$getregex('\\w'), 'The')
  twavel$getchar()
  expect_equal(twavel$getregex('[xis]'), 'six')
  expect_null(twavel$getregex('\\d'))
  expect_equal(twavel$getregex('.|\n'), "th\nsick\nsheik's\nsixth\nsheep's\nsick\n")
})

expect_line <- function(traversal, string) {
  eval(bquote(expect_equal(.(traversal$getline()), .(re_split(string, '')))))
}

test_that('getline()', {
  twavel <- traverse('../testfiles/tongue-twister.txt')
  expect_line(twavel, 'The')
  expect_line(twavel, 'sixth')
  expect_line(twavel, 'sick')
  expect_line(twavel, "sheik's")
  expect_line(twavel, 'sixth')
  expect_line(twavel, "sheep's")
  expect_line(twavel, 'sick')
})

test_that('putchar', { skip('not implemented') })
test_that('peek', { skip('not implemented') })
test_that('lookfor', { skip('not implemented') })
test_that('skipws', { skip('not implemented') })
test_that('expect', { skip('not implemented') })
