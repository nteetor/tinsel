context(' * testing scanner')

test_that('constructor', {
  police <- scanner('../testfiles/tiny.R')
  expect_s3_class(police, 'scanner')
  expect_true(is.scanner(police))

  expect_has_fields(police, 'stream', 'tokens')
  expect_has_methods(police, 'comment', 'comments', 'dcall', 'decoration',
                     'dreference', 'filename', 'identifier', 'nonsyntactic',
                     'syntactic', 'quotation', 'number', 'tokenize')

  expect_s3_class(police$tokens, 'stack')
  expect_s3_class(police$stream, 'traversal')
})

test_that('tokenize', {
  hp <- scanner('../testfiles/tiny.R')
  ticks <- hp$tokenize()
  expect_equal(length(ticks), 14)
  expect_equal(ticks, hp$tokens$tolist())
  types <- vapply(ticks, `[[`, numeric(1), 'type')
  values <- vapply(ticks, `[[`, character(1), 'value')
  expect_equal(types, c(.type$TINSEL_COMMENT, .type$FILE_REFERENCE,
                        .type$IDENTIFIER, .type$PACKAGE_ACCESSOR,
                        .type$IDENTIFIER, .type$IDENTIFIER, .type$RESERVED,
                        .type$RESERVED, .type$IDENTIFIER, .type$NUMBER,
                        .type$IDENTIFIER, .type$STRING,
                        .type$IDENTIFIER, .type$IDENTIFIER))
  expect_equal(values, c('#.', 'separate-file', 'pack', '::', 'deck', 'fun',
                         'function', '...',
                         'numeric_literal', '6.02e23',
                         'return value', "Hello, 'world'!",
                         'return', 'return value'))
})


