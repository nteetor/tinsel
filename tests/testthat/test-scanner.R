context(' * testing scanner')

test_that('constructor', {
  police <- scanner('../testfiles/tiny.R')

  expect_s3_class(police, 'scanner')
  expect_true(is.scanner(police))
  expect_fields(police, 'stream', 'tokens')
  expect_methods(police, 'comment', 'filename', 'identifier',
                 'nonsyntactic', 'syntactic', 'quotation', 'number',
                 'tokenize', 'assignment', 'expect', 'expression', 'extract')
  expect_s3_class(police$tokens, 'stack')
  expect_s3_class(police$stream, 'traversal')
})

test_that('tokenize', {
  hp <- scanner('../testfiles/tiny.R')
  ticks <- rev(as.list(hp$tokenize()))
  types <- type(ticks)
  contents <- contents(ticks)

  expect_equal(length(ticks), 18)
  expect_equal(types, c(
    .type$TINSEL_COMMENT,
    .type$FILE_REFERENCE,
    .type$IDENTIFIER,
    .type$PACKAGE_ACCESSOR,
    .type$IDENTIFIER,
    .type$IDENTIFIER,
    .type$ASSIGNMENT,
    .type$RESERVED,
    .type$RESERVED,
    .type$IDENTIFIER,
    .type$ASSIGNMENT,
    .type$NUMBER,
    .type$IDENTIFIER,
    .type$ASSIGNMENT,
    .type$STRING,
    .type$IDENTIFIER,
    .type$IDENTIFIER,
    .type$EOF
  ))
  expect_equal(contents, c(
    '#.',
    'separate-file',
    'pack',
    '::',
    'deck',
    'fun',
    '<-',
    'function',
    '...',
    'numeric_literal',
    '<-',
    '6.02e23',
    'return value',
    '<-',
    "Hello, 'world'!",
    'return',
    'return value',
    'EOF'
  ))
})
