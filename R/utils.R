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

re_search <- function(.string, pattern) {
  bounds <- regexpr(pattern, .string)
  if (bounds[[1]] != -1) {
    substr(.string, bounds[[1]], bounds[[1]] + attr(bounds, 'match.length') - 1)
  } else {
    NULL
  }
}

cat0 <- function(...) cat(..., sep = '')

is.try_error <- function(x) inherits(x, 'try-error')
