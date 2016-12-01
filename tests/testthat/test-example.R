context(' * testing example helpers')

test_that('file path exists', {
  expect_true(file.exists(tinsel_example('tags.R')))
})

test_that('error for incorrect name', {
  expect_error(tinsel_example('WHOOPS.R'))
})

test_that('`tinsel_examples` lists all files', {
  expect_equal(tinsel_examples(), c('attributes.R', 'tags.R', 'timer.R'))
})

