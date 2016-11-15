#' Read and Parse Decoratees from a File
#'
#' Given a file, \code{source_decoratees} reads and parses decorated functions
#' (decoratees) into the calling environment.
#'
#' @param file A character string specifying a file path.
#'
#' @details
#'
#' Malformed decoratees are ignored and a message will alert the user a function
#' has been skipped. However, an error is raised if a decorator is undefined.
#'
#' If you are working within RStudio the "Source Active File Decoratees" addin
#' effectively allows you to bind \code{source_decoratees} to a keyboard
#' shorcut. The addin is found under \bold{Tools} > \bold{Addins}.
#'
#' @export
source_decoratees <- function(file) {
  if (!file.exists(file)) {
    stop('file "', file, '" does not exist', call. = FALSE)
  }

  src <- new.env()
  tryCatch(
    source(file = file, local = src, keep.source = FALSE),
    error = function(e) {
      stop('problem sourcing ', file, ', ', e$message, call. = FALSE)
    }
  )

  fileitr <- file_itr(file)
  decor <- NULL

  while (fileitr$has_next()) {
    line <- fileitr$get_line()

    if (grepl('^\\s*#\\.', line)) {
      d <- gsub('#\\.|\\s', '', line)

      if (char_at(d, -1) != ')') {
        d <- paste0(d, '()')
      }

      decor <- c(decor, d)
    } else if (grepl('^(?!\\s*#).*<-\\s*function', line, perl = TRUE) && !is.null(decor)) {
      f <- gsub('^\\s*|\\s*<-.*$', '', line)
      decor <- rev(decor)

      as_text <- f
      for (d in decor) {
        split_at <- first_of(d, '(')
        dname <- substr(d, 1, split_at - 1)
        dargs <- substr(d, split_at + 1, nchar(d))
        if (!grepl('^\\s*\\)\\s*$', dargs)) {
          dargs <- paste(',', dargs)
        }

        if (!exists(dname, envir = src)) {
          stop('no definition found for decorator `', dname, '`', call. = FALSE)
        }

        as_text <- c(dname, '(', as_text, dargs)
      }

      text_call <- paste(as_text, collapse = '', sep = '')

      feval <- tryCatch(
        eval(parse(text = text_call), envir = src),
        error = function(e) {
          message('Problem evaluating `', f, '`: ', e$message)
          NULL
        }
      )

      if (!is.null(feval)) {
        class(feval) <- c('decorated', class(feval))
        attr(feval, 'decoratee') <- get0(f, src)
        dnames <- vapply(decor, re_search, character(1), '^\\s*[^(]+')
        attr(feval, 'decorators') <- setNames(lapply(dnames, get0, src), dnames)
        assign(f, feval, parent.frame())
      }

      decor <- NULL
    } else if (grepl('^\\s*$', line)) {
      # white space is frowned upon, but for now is accepted between decorator
      # lines

    } else {
      decor <- NULL
    }
  }
}
