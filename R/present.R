#' Load Decorated Functions
#'
#' Given an \code{R} file, the presents function parses and loads decorated
#' functions into the calling environment (or specified environment).
#'
#' @export
presents <- function(file, envir = as.environment(-1), verbose = TRUE) {
  if (!file.exists(file)) {
    stop('specified file ', file, ' does not exist', call. = FALSE)
  }

  src <- new.env()
  source(file = file, local = src, keep.source = TRUE)
  fileitr <- itr(file)

  functs <- new.env()
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

      fun <- get(f, envir = src, inherits = FALSE)
      attr(fun, 'decorators') <- decor
      functs[[f]] <- fun

      decor <- NULL
    } else if (grepl('^\\s*$', line)) {
      # white space is frowned upon, but accepted between decorators
    } else {
      decor <- NULL
    }
  }

  # mom's spaghetti
  for (nm in ls(functs)) {
    decos <- attr(functs[[nm]], 'decorators', exact = TRUE)

    fun <- function() NULL
    formals(fun) <- formals(functs[[nm]])
    body(fun) <- parse(text = paste0(
      paste0(c(decos, ''), collapse = '('),
      strrep(')', length(decos)),
      sep = ''
    ))
    environment(fun) <- envir

    print(fun)
  }
}
