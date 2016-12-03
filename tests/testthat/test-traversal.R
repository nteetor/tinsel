context(' * testing traversal')

test_that('initialize traversal', {
  twavel <- traversal('../testfiles/tongue-twister.txt')
  expect_s3_class(twavel, 'traversal')
  expect_true(is.traversal(twavel))
  expect_type(twavel$cursor, 'double')
  expect_equal(twavel$cursor, 1)
  expect_type(twavel$chars, 'character')
  expect_equal(length(twavel$chars), 43)

  expect_has_fields(twavel, 'cursor', 'chars', 'EOF', 'EOL')

  expect_has_methods(twavel, 'at_eof', 'size', 'reset', 'increment_cursor',
                     'decrement_cursor', 'hasline', 'getchar', 'unget',
                     'getregex', 'getline', 'peek', 'skipws', 'expect')
})

test_that('increment_cursor()', {
  twavel <- traversal('../testfiles/tongue-twister.txt')
  expect_equal(twavel$cursor, 1)
  twavel$increment_cursor()
  expect_equal(twavel$cursor, 2)
  twavel$increment_cursor()
  expect_equal(twavel$cursor, 3)
  for (i in seq_len(41)) twavel$increment_cursor()
  expect_equal(twavel$cursor, 43)
})

test_that('decrement_cursor()', {
  twavel <- traversal('../testfiles/tongue-twister.txt')
  expect_equal(twavel$cursor, 1)
  twavel$decrement_cursor()
  expect_equal(twavel$cursor, 1)
  twavel$increment_cursor()
  twavel$increment_cursor()
  twavel$decrement_cursor()
  expect_equal(twavel$cursor, 2)
})

test_that('hasline()', {
  twavel <- traversal('../testfiles/tongue-twister.txt')
  expect_true(twavel$hasline())

  expect_line(twavel, 'The')
  expect_true(twavel$hasline())

  expect_line(twavel, 'sixth')
  expect_true(twavel$hasline())

  expect_line(twavel, 'sick')
  expect_true(twavel$hasline())

  expect_line(twavel, "sheik's")
  expect_true(twavel$hasline())

  expect_line(twavel, 'sixth')
  expect_true(twavel$hasline())

  expect_line(twavel, "sheep's")
  expect_true(twavel$hasline())

  expect_line(twavel, 'sick')
  expect_false(twavel$hasline())
})

test_that('getchar()', {
  twavel <- traversal('../testfiles/tongue-twister.txt')
  expect_char(twavel, 'T')
  expect_char(twavel, 'h')
  expect_char(twavel, 'e')
  expect_char(twavel, '\n')
  expect_char(twavel, 's')
  for (i in seq_len(twavel$size())) twavel$getchar()
  expect_char(twavel, 'EOF')
})

test_that('getregex()', {
  twavel <- traversal('../testfiles/tongue-twister.txt')
  expect_equal(twavel$getregex('\\w'), 'The')
  twavel$getchar()
  expect_equal(twavel$getregex('[xis]'), 'six')
  expect_null(twavel$getregex('\\d'))
  expect_equal(twavel$getregex('.|\n'), "th\nsick\nsheik's\nsixth\nsheep's\nsick\n")
})

test_that('getline()', {
  twavel <- traversal('../testfiles/tongue-twister.txt')
  expect_line(twavel, 'The')
  expect_line(twavel, 'sixth')
  expect_line(twavel, 'sick')
  expect_line(twavel, "sheik's")
  expect_line(twavel, 'sixth')
  expect_line(twavel, "sheep's")
  expect_line(twavel, 'sick')
})

test_that('peek()', {
  twavel <- traversal('../testfiles/tongue-twister.txt')
  expect_equal(twavel$peek(), 'T')
  expect_char(twavel, 'T')
  expect_equal(twavel$peek(), 'h')
  expect_line(twavel, 'he')
  expect_line(twavel, 'sixth')
  expect_equal(twavel$peek(), 's')
  expect_char(twavel, 's')
  expect_equal(twavel$peek(), 'i')
  while (twavel$hasline()) twavel$getline()
  expect_equal(twavel$peek(), 'EOF')
})

test_that('unget()', {
  twavel <- traversal('../testfiles/tongue-twister.txt')
  expect_char(twavel, 'T')
  twavel$unget()
  expect_equal(twavel$peek(), 'T')
  expect_line(twavel, 'The')
  twavel$unget()
  expect_char(twavel, '\n')
  expect_char(twavel, 's')
  expect_char(twavel, 'i')
})

test_that('as_eof()', {
  twavel <- traversal('../testfiles/tongue-twister.txt')
  expect_false(twavel$at_eof())
  for (i in seq_len(twavel$size())) twavel$getchar()
  expect_true(twavel$at_eof())
})

test_that('reset()', {
  twavel <- traversal('../testfiles/tongue-twister.txt')
  while (twavel$hasline()) twavel$getline()
  expect_equal(twavel$getchar(), 'EOF')
  expect_true(twavel$at_eof())
  expect_equal(twavel$cursor, twavel$size())
  expect_silent(twavel$reset())
  expect_equal(twavel$cursor, 1)
})

test_that('skipws()', {
  twavel <- traversal('../testfiles/r-function.utils')
  twavel$getline()
  twavel$skipws()
  expect_char(twavel, 'f')
  expect_char(twavel, 'u')
})

test_that('expect()', {
  twavel <- traversal('../testfiles/tongue-twister.txt')
  expect_error(twavel$expect('@'), 'found "T" on line 1, expected "@"')
  expect_silent(twavel$expect('h'))
})
