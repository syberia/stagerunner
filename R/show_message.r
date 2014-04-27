#' Show a progress message.
#'
#' @param stage_names character.
#' @param stage_index integer.
#' @param begin logical. Whether we are showing the begin or end message.
#' @param nested logical. Whether or not this is a nested stage (i.e.
#'    contains another stageRunner).
#' @param depth integer. How many tabs to space by (for nested stages).
#' @importFrom testthat colourise
#' @return the message to standard output
#' @examples 
#' \dontrun{
#' show_message(c('one', 'two'), 2) # Will print "Beginning one stage..."
#' }
show_message <- function(stage_names, stage_index, begin = TRUE, nested = FALSE, depth = 1) {
  stage_name <- stage_names[stage_index]
  if (is.null(stage_name) || stage_name == "")
    stage_name <- list('first', 'second', 'third')[stage_index][[1]] %||%
                  paste0(stage_index, 'th')
  stage_name <- testthat:::colourise(stage_name,
                                     if (begin) 'green' else 'blue')
  stage_name <- gsub("(?:Begin|Run) (.*) stage(\\.{3})?", "\\1", stage_name)
  depth <- paste(rep("  ", depth - 1), collapse = '')
  if (nested) cat(paste0(depth, if (begin) "Beginn" else "End", "ing ", stage_name, " stage...\n"))
  else if (begin) cat(paste0(depth, "Running ", stage_name, "...\n"))
}
