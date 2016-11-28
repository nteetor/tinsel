context('testing addins')

test_that('source_decoratees addin', {
  expect_error(source_decoratees_addin(), 'addin available only .* RStudio$')
})

test_that('source_selection addin', {
  expect_error(source_selection_addin(), 'addin available only .* RStudio$')
})
