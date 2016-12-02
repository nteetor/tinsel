context(' * testing scanner')

test_that('constructor', {
  police <- scanner('../testfiles/tiny.R')

  expect_has_fields(police, 'stream', 'tokens')
  expect_has_methods(police, 'comment', 'comments', 'dcall', 'decoration',
                     'dreference', 'filename', 'identifier', 'nonsyntactic',
                     'syntactic', 'tokenize')

  expect_s3_class(police$tokens, 'stack')
  expect_s3_class(police$stream, 'traversal')
})

test_that('tokenize', {
  hp <- scanner('../testfiles/tiny.R')
  ticks <- hp$tokenize()
  expect_equal(length(ticks), 7)
  expect_equal(ticks, hp$tokens$tolist())
  types <- vapply(ticks, `[[`, numeric(1), 'type')
  values <- vapply(ticks, `[[`, character(1), 'value')
  expect_equal(types, c(.type$TINSEL_COMMENT, .type$FILE_REFERENCE,
                        .type$IDENTIFIER, .type$IDENTIFIER, .type$RESERVED,
                        .type$IDENTIFIER, .type$IDENTIFIER))
  expect_equal(values, c('#.', 'separate-file', 'decorator', 'fun', 'function',
                         'return', 'value'))
})


