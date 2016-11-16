# tinsel example file
#   @date Nov. 16, 2016
#   @author nteetor
#   @description Use decorators to format text with HTML tags.

#' HTML Tags Decorator
#'
#' A decorator to format text with HTML tags.
#'
#' @param f A function.
#' @param name A class name.
#'
#' @return
#'
#' The value of calling \code{f}.
#'
html_wrap <- function(f, tag) {
  function(...) {
    sprintf('<%s>%s</%s>', tag, f(...), tag)
  }
}

#. html_wrap('p')
html_paragraph <- function(text) {
  as.character(text)
}

#. html_wrap('b')
html_bold <- function(text) {
  as.character(text)
}

#. html_wrap('p')
#. html_wrap('i')
html_block_quote <- function(text, author) {
  paste0('"', as.character(text), '" -', author)
}
