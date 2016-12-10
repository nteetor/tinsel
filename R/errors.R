# borrowed from *Advanced R*
condition <- function(subclass, message, call = sys.call(-1), ...) {
  structure(
    list(
      message = message,
      call = call
    ),
    class = c(subclass, 'error', 'condition'),
    ...
  )
}

elaborate <- function(symbol) {
  if (is.character(symbol)) {
    if (.sym$NUMBER == symbol) {
      'a numeric character'
    } else if (.sym$LETTER == symbol) {
      'an alphabetic character'
    } else if (.sym$FILENAME_CHAR == symbol) {
      'a character allowed in a file name' # bleh
    } else if (.sym$SYNTACTIC_CHAR == symbol) {
      'an alphanumeric character, "_", or "."'
    } else {
      paste0('"', symbol, '"')
    }
  } else if (symbol %in% .type) {
    if (symbol == .type$TINSEL_COMMENT) {
      'a tinsel comment'
    } else {
      names(which(.type == symbol))
    }
  } else {
    stop("cannot elaborate upon ", symbol, ", you're on your own", call. = FALSE)
  }
}

expected <- function(symbol, actual, lineno, ...) {
  msg <- sprintf('found "%s" on line %i, expected %s', actual, lineno,
                 elaborate(symbol))
  condition('expected', msg, call = NULL, ...)
}
