.sym <- list(
  EOF = 'EOF',
  EOL = '\n',
  COMMENT = '#',
  TINSEL_COMMENT = '#\\.',
  LPAREN = '\\(',
  RPAREN = '\\)',
  LESSTHAN = '<',
  MINUS = '-',
  EQUALS = '=',
  NUMBER = '[0-9]',
  LETTER = '[a-zA-Z]',
  PERIOD = '.',
  TICK = '`',
  APOSTROPHE = "'",
  DOLLARSIGN = '$',
  FILENAME_CHAR = '[^/\\?%*:|"<>]',
  SYNTACTIC_CHAR = '[a-zA-Z0-9_.]'
)

.type <- list(
  FILE_REFERENCE = 1,
  FILE_INDEXING = 2,
  DECORATOR = 3,
  DECORATEE = 4
)
