context(' * testing addins')

test_that('rstudioapi package is installed', {
  skip_on_cran()
  skip_if_not(interactive(), 'not tested while interactive')

  is_installed <- 'rstudioapi' %in% rownames(installed.packages())
  if (is_installed) {
    remove.packages('rstudioapi')
  }
  on.exit({
    if (is_installed) {
      install.packages('rstudioapi', repos = "https://cran.rstudio.com/", quiet = TRUE)
    }
  })

  expect_error(source_decoratees_addin(), 'please install the "rstudioapi" package')
  expect_error(source_selection_addin(), 'please install the "rstudioapi" package')
})

test_that('must be working in RStudio', {
  skip_if_not_installed('rstudioapi')

  expect_error(source_decoratees_addin(), 'addin available only .* RStudio$')
  expect_error(source_selection_addin(), 'addin available only .* RStudio$')
})
