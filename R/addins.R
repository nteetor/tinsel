#' Tinsel RStudio Addins
#'
#' tinsel provides two RStudio addins to make working with decorators more
#' intuitive. The first "Source Active File Decoratees" will source decorated
#' functions from the current file in your RStudio session. The second "Run
#' Decoratees Selected Line(s)" behaves like RStudio's run lines command.
#' Decoratored functions within the highlighted section are parsed into the
#' global environment.
#'
#' @name addins
NULL

source_decoratees_addin <- function() {
  if (!requireNamespace('rstudioapi')) {
    stop('please install the rstudioapi package', call. = FALSE)
  }

  active <- tryCatch(
    rstudioapi::getSourceEditorContext(),
    error = function(e) {
      list(
        id = '',
        path = '',
        contents = ''
      )
    })

  if (active$path != '') {
    cmd <- paste0('source_decoratees("', active$path, '")')
    rstudioapi::sendToConsole(cmd)
  }
}

decorate_selected_lines <- function() {
  context <- rstudioapi::getSourceEditorContext()
  selection <- rstudioapi::primary_selection(context)
  selected_text <- selection$text
  tryCatch(
    source_decoratees(textConnection(selected_text), into = globalenv()),
    error = function(e) {
      message('Error :', e$message)
    })
}
