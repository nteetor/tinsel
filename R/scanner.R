scanner <- function(file) {
  self <- new.env(parent = emptyenv())
  self$tokens <- stack()
  self$stream <- traverse(file)

  self$comments <- function() {
    while (self$stream$peek() == .sym$COMMENT) {
      self$comment()

      # skip over leading white space before a comment
      self$stream$skipws()
    }
  }
  self$comment <- function() {
    self$stream$expect(.sym$COMMENT)
    if (self$stream$peek() == .sym$PERIOD) {
      # tinsel comment
      self$stream$expect(.sym$PERIOD)
      self$tokens$push(token(paste0(.sym$COMMENT, .sym$PERIOD), .type$TINSEL_COMMENT))
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
      self$stream$expect(.sym$LBRACKET)
      self$filename()
      self$stream$expect(.sym$RBRACKET)
    }
  }
  self$filename <- function() {
    buffer <- NULL
    while (re_match(self$stream$peek(), .sym$FILENAME_CHAR)) {
      buffer <- paste0(buffer, self$stream$getchar())
    }
    self$tokens$push(token(buffer, .type$FILE_REFERENCE))
  }
  self$dcall <- function() {
    self$identifier()
    if (self$stream$peek() == .sym$COLON) {
      self$stream$expect(.sym$COLON)
      self$stream$expect(.sym$COLON)
      self$tokens$push(token(strrep(.sym$COLON, 2), .type$PACKAGE_ACCESSOR))

      self$identifier()
    }
  }
  self$identifier <- function() {
    if (self$stream$peek() == .sym$BACKTICK) {
      self$quoted()
    } else {
      self$syntactic()
    }
  }
  self$quoted <- function() {
    self$stream$expect(.sym$BACKTICK)
    buffer <- NULL
    while (self$stream$peek() != .sym$BACKTICK) {
      buffer <- paste0(buffer, self$stream$getchar())
    }
    self$tokens$push(token(buffer, .type$IDENTIFIER))
    self$stream$expect(.sym$BACKTICK)
  }
  self$syntactic <- function() {
    buffer <- NULL
    while (re_match(self$stream$peek(), .sym$SYNTACTIC_CHAR)) {
      buffer <- paste0(buffer, self$stream$getchar())
    }
    self$tokens$push(token(buffer, .type$IDENTIFIER))
  }

  self$tokenize <- function() {
    while ((c <- self$stream$getchar()) != .sym$EOF) {
      if (c == .sym$COMMENT) {
        self$stream$putchar(c)
        self$comments()
      } else if (re_match(c, .sym$IDENTIFIER_CHAR)) {
        self$stream$putchar(c)
        self$identifier()
      }
    }

    self$tokens$tolist()
  }

  self
}
