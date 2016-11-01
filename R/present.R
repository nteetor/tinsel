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
#' If any decorators are not defined or if a function is not defined properly
#' none of the functions parsed are loaded.
#'
#' The name of the function is *very* likely to change.
#'
#' @export
presents <- function(file, envir, verbose = FALSE) {
  if (!file.exists(file)) {
    stop('file "', file, '" does not exist', call. = FALSE)
  }

  src <- new.env()
  source(file = file, local = src, keep.source = TRUE)
  fileitr <- itr(file)

  functs <- list()
  decor <- NULL

  while (fileitr$has_next()) {
    line <- fileitr$get_line()

    if (grepl('^\\s*#\\.', line)) {
      d <- gsub('#\\.|\\s', '', line)
      decor <- c(decor, d)
    } else if (grepl('<-\\s*function', line) && !is.null(decor)) {
      f <- gsub('^\\s*|\\s*<-.*$', '', line)

      if (!exists(f, envir = src, inherits = FALSE) && verbose) {
        message('skipping function ', f)
      }

      for (dfunc in decor) {
        if (!exists(dfunc, envir = src, inherits = FALSE)) {
          stop('no definition found for decorator `', dfunc, '`', call. = FALSE)
        }
      }

      as_text <- paste(c(decor, f), collapse = '(', sep = '')
      parens_closed <- paste0(as_text, strrep(')', length(decor)), sep = '')
      functs[[f]] <- parens_closed

      decor <- NULL
    } else if (grepl('^\\s*$', line)) {
      # white space is frowned upon, but for now is accepted between decorator
      # lines
    } else {
      decor <- NULL
    }
  }

  for (nm in names(functs)) {
    envir[[nm]] <- eval(parse(text = functs[[nm]]), envir = src)
  }
}
