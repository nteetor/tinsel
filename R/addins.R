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
  if (!requireNamespace('rstudioapi', quietly = TRUE)) {
    stop('RStudio addins require package "rstudioapi"', call. = FALSE)
  }

  if (!rstudioapi::isAvailable()) {
    stop('addin available only while working within RStudio', call. = FALSE)
  }

  active_path <- (rstudioapi::getSourceEditorContext())$path

  if (active_path == '') {
    return()
  }

  console_cmd <- paste0('source_decoratees("', active_path, '")')

  rstudioapi::sendToConsole(console_cmd)
}

source_selection_addin <- function() {
  if (!requireNamespace('rstudioapi', quietly = TRUE)) {
    stop('RStudio addins require package "rstudioapi"', call. = FALSE)
  }

  if (!rstudioapi::isAvailable()) {
    stop('addin available only while working within RStudio', call. = FALSE)
  }

  editor_context <- rstudioapi::getSourceEditorContext()
  file_selection <- rstudioapi::primary_selection(editor_context)
  selected_text <- file_selection$text

  if (grepl('^\\s*$', selected_text)) {
    console_cmd <- ''
  } else {
    console_cmd <- paste0('source_decoratees(textConnection("\n', selected_text, '\n"))')
  }

  rstudioapi::sendToConsole(console_cmd)
}
