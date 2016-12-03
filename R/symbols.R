.sym <- list(
  EOF = 'EOF',
  EOL = '\n',
  COMMENT = '#',
  LPAREN = '(',
  RPAREN = ')',
  LBRACKET = '[',
  RBRACKET = ']',
  LESSTHAN = '<',
  MINUS = '-',
  COLON = ':',
  PERIOD = '.',
  BACKTICK = '`',
  SINGLEQUOTE = "'",
  DOUBLEQUOTE = '"',
  DOLLARSIGN = '$',
  BACKSLASH = '\\',
  NUMBER = '[0-9]',
  LETTER = '[a-zA-Z]',
  # when using decorators from separate files:
  #   1. the file must not contain a right brace
  #   2. we assume the operating system will impose file name requirements
  #   3. hopefully this allows filenames of different locales
  FILENAME_CHAR = '[^\\]]',
  # syntactic variable names, see ?Quotes
  SYNTACTIC_CHAR = '[\\w.]',
  # quoted characters are anything except a backtick
  QUOTED_CHAR = '[^`]',
  IDENTIFIER_CHAR = '[`\\w.]'
)

.type <- list(
  FILE_REFERENCE = 1,
  FILE_INDEXER = 2,
  DECORATOR = 3,
  IDENTIFIER = 4,
  PACKAGE_ACCESSOR = 5,
  TINSEL_COMMENT = 6,
  RESERVED = 7,
  STRING = 8,
  UNKNOWN = 99
)

.reserved <- c('if', 'else', 'repeat', 'while', 'function', 'for', 'in',
               'next', 'break', 'TRUE', 'FALSE', 'NULL', 'Inf', 'NaN', 'NA',
               'NA_integer_', 'NA_real_', 'NA_complex_', 'NA_character_', '...')
