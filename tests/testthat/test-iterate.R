context('testing file iterator')

test_that('file must exist', {
  expect_error(file_itr('does/not/exist.R'), 'could not find file')
})

test_that('all lines read', {
  fitr <- file_itr('../testfiles/tongue-twister.txt')
  expect_equal(length(fitr$contents), 7)
})

test_that('get lines and has lines', {
  fitr <- file_itr('../testfiles/tongue-twister.txt')

  expect_equal(fitr$get_line(), 'The')
  expect_equal(fitr$get_line(), 'sixth')
  expect_equal(fitr$get_line(), 'sick')
  expect_equal(fitr$get_line(), "sheik's")
  expect_equal(fitr$get_line(), 'sixth')
  expect_equal(fitr$get_line(), "sheep's")

  expect_true(fitr$has_next())

  expect_equal(fitr$get_line(), 'sick')
  expect_error(fitr$get_line(), 'at end of file')

  expect_false(fitr$has_next())
})

