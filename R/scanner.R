is.scanner <- function(x) inherits(x, 'scanner')

scanner <- function(file) {
  self <- new.env(parent = emptyenv())
  self$tokens <- stack()
  self$stream <- traversal(file)

  self$expect <- function(symbol) {
    c <- self$stream$getchar()
    if (c != symbol) {
      stop(expected(symbol, c, self$stream$lineno()))
    } else {
      c
    }
  }

  self$comments <- function() {
    while (self$stream$peek() == .sym$COMMENT) {
      self$comment()

      # skip over leading white space before a comment
      self$stream$skipws()
    }
  }
  self$comment <- function() {
    self$expect(.sym$COMMENT)
    if (self$stream$peek() == .sym$PERIOD) {
      # tinsel comment
      self$expect(.sym$PERIOD)
      self$tokens$push(token(.sym$TINSEL_COMMENT, .type$TINSEL_COMMENT,
                             self$stream$lineno()))
      self$decoration()
    }

    # digest characters through end of line (including newline)
    self$stream$getline()
  }
  self$decoration <- function() {
    self$stream$skipws()
    self$dreference()
    self$stream$skipws()
    self$dcall()
  }
  self$dreference <- function() {
    if (self$stream$peek() == .sym$LBRACKET) {
      self$expect(.sym$LBRACKET)
      self$filename()
      self$expect(.sym$RBRACKET)
    }
  }
  self$filename <- function() {
    buffer <- NULL
    while (re_match(self$stream$peek(), .sym$FILENAME_CHAR)) {
      buffer <- paste0(buffer, self$stream$getchar())
    }
    self$tokens$push(token(buffer, .type$FILE_REFERENCE, self$stream$lineno()))
  }
  self$dcall <- function() {
    self$identifier()
    if (self$stream$peek() == .sym$COLON) {
      self$expect(.sym$COLON)
      self$expect(.sym$COLON)
      self$tokens$push(token(.sym$PACKAGE_ACCESSOR, .type$PACKAGE_ACCESSOR,
                             self$stream$lineno()))

      self$identifier()
    }
  }
  self$identifier <- function() {
    if (self$stream$peek() == .sym$BACKTICK) {
      self$nonsyntactic()
    } else {
      self$syntactic()
    }
  }
  self$nonsyntactic <- function() {
    self$expect(.sym$BACKTICK)
    buffer <- NULL
    while (self$stream$peek() != .sym$BACKTICK) {
      buffer <- paste0(buffer, self$stream$getchar())
    }
    self$tokens$push(token(buffer, .type$IDENTIFIER, self$stream$lineno()))
    self$expect(.sym$BACKTICK)
  }
  self$syntactic <- function() {
    buffer <- NULL
    while (re_match(self$stream$peek(), .sym$SYNTACTIC_CHAR)) {
      buffer <- paste0(buffer, self$stream$getchar())
    }
    if (buffer %in% .reserved) {
      self$tokens$push(token(buffer, .type$RESERVED, self$stream$lineno()))
    } else {
      self$tokens$push(token(buffer, .type$IDENTIFIER, self$stream$lineno()))
    }
  }
  self$quotation <- function() {
    # this may seem odd, but is necessary
    if (self$stream$peek() == .sym$SINGLEQUOTE) {
      qtype <- .sym$SINGLEQUOTE
    } else {
      # here is where we catch a potential incorrect character by assuming
      # if not single quote then double quote
      qtype <- .sym$DOUBLEQUOTE
    }

    self$expect(qtype)
    buffer <- NULL
    while (self$stream$peek() != qtype) {
      c <- self$stream$getchar()
      if (c == .sym$BACKSLASH && self$stream$peek() == qtype) {
        buffer <- paste0(buffer, c, self$stream$getchar())
      } else {
        buffer <- paste0(buffer, c)
      }
    }
    self$expect(qtype)
    self$tokens$push(token(buffer, .type$STRING, self$stream$lineno()))
  }
  self$number <- function() {
    buffer <- NULL
    while (re_match(self$stream$peek(), .sym$NUMBER)) {
      if (self$stream$peek() == .sym$PERIOD &&
          first_of(buffer, .sym$PERIOD) != -1) {
        self$expect(.sym$NUMBER)
      } else if (self$stream$peek() == .sym$EXPNOTATION &&
          first_of(buffer, .sym$EXPNOTATION) != -1) {
        self$expect(.sym$NUMBER)
      } else {
        buffer <- paste0(buffer, self$stream$getchar())
      }
    }
    self$tokens$push(token(buffer, .type$NUMBER, self$stream$lineno()))
  }
  self$assignment <- function() {
    c <- self$expect(.sym$LESSTHAN)
    if (self$stream$peek() == .sym$MINUS) {
      buffer <- paste0(c, self$expect(.sym$MINUS))
      self$tokens$push(token(buffer, .type$ASSIGNMENT, self$stream$lineno()))
    }
  }

  self$tokenize <- function() {
    self$stream$reset()
    self$tokens <- stack()

    while (self$stream$peek() != .sym$EOF) {
      c <- self$stream$peek()
      if (c == .sym$COMMENT) {
        self$comments()
      } else if (re_match(c, .sym$IDENTIFIER_CHAR)) {
        self$identifier()
      } else if (c == .sym$SINGLEQUOTE || c == .sym$DOUBLEQUOTE) {
        self$quotation()
      } else if (re_match(c, .sym$NUMBER)) {
        self$number()
      } else if (c == .sym$LESSTHAN) {
        self$assignment()
      } else {
        self$stream$getchar()
      }
    }
    self$tokens$push(token(.sym$EOF, .type$EOF, self$stream$lineno()))

    self$tokens
  }

  class(self) <- 'scanner'
  self
}
