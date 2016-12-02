`%||%` <- function(a, b) if (is.null(a)) b else a

char_at <- function(.string, i) {
  if (abs(i) > nchar(.string) || i == 0) stop('index out of bounds', call. = FALSE)
  j <- (i %% nchar(.string)) + (i < 0)
  substr(.string, j, j)
}

first_of <- function(.string, c) {
  if (!grepl(c, .string, fixed = TRUE)) return(-1)
  i <- 1
  while (i < nchar(.string) && char_at(.string, i) != c) i <- i + 1
  i
}

re_split <- function(.string, pattern) {
  strsplit(.string, pattern, perl = TRUE)[[1]]
}

re_search <- function(.string, pattern) {
  bounds <- regexpr(pattern, .string)
  if (bounds[[1]] != -1) {
    substr(.string, bounds[[1]], bounds[[1]] + attr(bounds, 'match.length') - 1)
  } else {
    NULL
  }
}

re_match <- function(.string, pattern) {
  grepl(paste0('^', pattern, '$'), .string, perl = TRUE)
}

set_names <- function(obj, nm) {
  if (missing(nm)) nm <- vapply(obj, as.character, character(1))
  names(obj) <- nm
  obj
}

cat0 <- function(...) cat(..., sep = '')

is.connection <- function(x) inherits(x, 'connection')
