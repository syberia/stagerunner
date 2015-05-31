#' Append one stageRunner to the end of another.
#'
#' @name stageRunner_append
#' @param other_runner stageRunner. Another stageRunner to append to the current one.
#' @param label character. The label for the new stages (this will be the name of the
#'   newly appended list element).
stageRunner_append <- function(other_runner, label = NULL) {
  stopifnot(is.stagerunner(other_runner))
  new_stage <- structure(list(other_runner), names = label)
  self$stages <- base::append(self$stages, new_stage)
  TRUE
}

