## Consider our example stagerunner from before:
##
##   * import data
##   * clean data
##      * impute variable 1
##      * discretize variable 2
##   * train model
##
## Our goal is to display progress when executing the stagerunner:
##
## ![runner](http://i.imgur.com/NKN3hnk.png)
##
#' Show a progress message when executing a stagerunner.
#'
#' @name show_message
#' @param stage_names character.
#' @param stage_index integer.
#' @param begin logical. Whether we are showing the begin or end message.
#' @param nested logical. Whether or not this is a nested stage (i.e.
#'    contains another stageRunner).
#' @param depth integer. How many tabs to space by (for nested stages).
#' @return Nothing, but print the message to standard output.
#' @examples 
#' \dontrun{
#'   show_message(c('one', 'two'), 2) # Will print "Beginning one stage..."
#' }
show_message <- function(stage_names, stage_index, begin = TRUE,
                         nested = FALSE, depth = 1) {
  stage_name <- stage_names[stage_index]

  ## If the stage was not named (i.e., only a function was given), we "impute"
  ## the name with an ordinal: "fifth", "twelfth", "21st", etc. (depending on
  ## the index of the stage).
  if (is.null(stage_name) || identical(stage_name, "") || identical(stage_name, NA_character_)) {
    stage_name <- as.ordinal(stage_index)
  }

  if (begin) {
    stage_name <- crayon::green(stage_name)
  } else {
    stage_name <- crayon::blue(stage_name)
  }

  ## We indent by `depth` double-spaces to show nested stages clearly.
  prefix     <- paste(rep("  ", depth - 1), collapse = '')
  ## We turn "import data" into "1. import data".
  stage_name <- paste0(stage_index, ". ", stage_name)

  ## Non-terminal stages (i.e., those with more sub-stages) have a beginning
  ## and an ending, so we show "Beginning 2. clean data stage" and
  ## "Ending 2. clean data stage".
  if (nested) {
    cat(paste0(prefix, if (begin) "Beginn" else "End", "ing ",
               stage_name, " stage...\n"))
  } else if (begin) {
    ## Whereas terminal stages (i.e., those without sub-stages) just *run*,
    ## so we show "Running 1. import data stage".
    cat(paste0(prefix, "Running ", stage_name, "...\n"))
  }
}

