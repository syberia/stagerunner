## Imagine we have *two* stagerunners:
##
##   * Import data
##   * Clean data
##
## and
## 
##   * Train model
##   * Export model
##
## It is a natural operation to *concatenate* or append these stagerunners
## into a single runner. We can do this using the `$append` method.
##
## However, append will create one final stage at the end instead of
## juxtaposing the stages, to make it clear which runner was appended.
##
#' Append one stageRunner to the end of another.
#'
#' @name stageRunner_append
#' @param other_runner stageRunner. Another stageRunner to append to the current one.
#' @param label character. The label for the new stages (this will be the name of the
#'   newly appended list element).
stageRunner_append <- function(other_runner, label = NULL) {
  stopifnot(is.stagerunner(other_runner))
  new_stage <- structure(list(other_runner), names = label)
  ## Appending a stagerunner is simply concatenating its stages.
  self$stages <- c(self$stages, new_stage)
  self
}


