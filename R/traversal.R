is.traversal <- function(x) inherits(x, 'traversal')

traversal <- function(file) {
  fsize <- (file.info(file))$size
  ffile <- file(file, open = 'r')

  self <- new.env(parent = baseenv())
  self$chars <- c(strsplit(readChar(ffile, nchars = fsize), '')[[1]],
                  .sym$EOF)
  close(ffile)

  self$cursor <- 1
  self$EOF <- .sym$EOF
  self$EOL <- .sym$EOL

  self$size <- function() {
    length(self$chars)
  }
  self$at_eof <- function() {
    self$cursor >= self$size()
  }
  self$lineno <- function() {
    sum(self$chars[1:self$cursor] == self$EOL, na.rm = TRUE) + 1
  }
  self$reset <- function() {
    self$cursor <- 1
  }
  self$increment_cursor <- function() {
    self$cursor <- min(self$cursor + 1, self$size())
  }
  self$decrement_cursor <- function() {
    self$cursor <- max(self$cursor - 1, 1)
  }
  self$peek <- function() {
    if (self$at_eof()) return(self$EOF)
    self$chars[self$cursor]
  }
  self$hasline <- function() {
    any(self$chars[self$cursor:self$size()] == self$EOL) && !self$at_eof()
  }

  self$getchar <- function() {
    if (self$at_eof()) return(self$EOF)

    c <- self$chars[self$cursor]
    self$increment_cursor()
    c
  }
  self$unget <- function() {
    # currently no need to support putting back a character
    self$decrement_cursor()
  }
  self$getregex <- function(regex) {
    buffer <- NULL
    while (re_match((c <- self$getchar()), regex) && c != self$EOF) {
      buffer <- paste0(buffer, c)
    }
    self$decrement_cursor()
    buffer
  }
  self$getline <- function() {
    buffer <- NULL
    while ((c <- self$getchar()) != self$EOL && c != self$EOF) {
      buffer <- c(buffer, c)
    }
    buffer
  }

  self$skipws <- function() {
    while (re_match(self$peek(), .sym$WHITESPACE)) self$increment_cursor()
  }

  class(self) <- 'traversal'
  self
}
