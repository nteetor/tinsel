#' Load Decorated Functions
#'
#' Given an \code{R} file, this function parses and loads decorated functions
#' into a specified environment.
#'
#' @param file A character string specifying a file path.
#' @param envir The \code{\link{environment}} where parsed functions are
#'   assigned.
#' @param verbose Defaults to \code{FALSE}, if \code{TRUE} print progress
#'   messages.
#'
#' @details
#'
#' If any decorators are undefined or if a function is not defined properly
#' none of the decorated functions parsed are loaded.
#'
#' @export
source_decorated <- function(file) {
  if (!file.exists(file)) {
    stop('file "', file, '" does not exist', call. = FALSE)
  }

  envir <- parent.frame()

  src <- new.env()
  source(file = file, local = src, keep.source = TRUE)
  fileitr <- itr(file)

#  functs <- list()
  decor <- NULL

  # f
  # another(f, "foo")
  # excite(another(f, "foo"), 5)

  while (fileitr$has_next()) {
    line <- fileitr$get_line()

    if (grepl('^\\s*#\\.', line)) {
      d <- gsub('#\\.|\\s', '', line)

      if (char_at(d, -1) != ')') {
        d <- paste0('()')
      }

      decor <- c(decor, d)
    } else if (grepl('<-\\s*function', line) && !is.null(decor)) {
      f <- gsub('^\\s*|\\s*<-.*$', '', line)

      if (!exists(f, envir = src, inherits = FALSE)) {
        message('skipping function ', f)
      }

      as_text <- paste0(f, '(', pairstring(src[[f]]), ')')
      for (d in decor) {
        if (!exists(d, envir = src, inherits = FALSE)) {
          stop('no definition found for decorator `', d, '`', call. = FALSE)
        }
        i <- first_of(d, '(')
        as_text <- c(substr(d, 1, i), as_text, substr(d, i + i, nchar(d)))
      }

#      as_text <- paste(c(decor, f), collapse = '(', sep = '')
#      parens_closed <- paste0(as_text, strrep(')', length(decor)), sep = '')

      tryCatch(
        assign(
          f,
          eval(parse(text = as_text), envir = src),
          envir = parent.frame()
        ),
        error = function(e) {
          message('Problem evaluating `', f, '`: ', e$message)
        }
      )

      decor <- NULL
    } else if (grepl('^\\s*$', line)) {
      # white space is frowned upon, but for now is accepted between decorator
      # lines
    } else {
      decor <- NULL
    }
  }
}
