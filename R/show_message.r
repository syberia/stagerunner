#' Show a progress message.
#'
#' @param stage_names character.
#' @param stage_index integer.
#' @param begin logical. Whether we are showing the begin or end message.
#' @importFrom testthat colourise
#' @return the message to standard output
#' @examples 
#' \dontrun{
#' show_message(c('one', 'two'), 2) # Will print "Beginning one stage..."
#' }
show_message <- function(stage_names, stage_index, begin = TRUE) {
  stage_name <- stage_names[stage_index]
  if (is.null(stage_name) || stage_name == "")
    stage_name <- list('first', 'second', 'third')[stage_index][[1]] %||%
                  paste0(stage_index, 'th')
  stage_name <- testthat:::colourise(paste0(stage_name, '...'),
                                     if (begin) 'green' else 'blue')
  stage_name <- gsub("(?:Begin|Run) (.*) stage(\\.{3})?", "\\1", stage_name)
  cat(paste0(if (begin) "Beginn" else "End", "ing ", stage_name, " stage...\n"))
}
