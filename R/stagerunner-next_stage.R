#' For stageRunners with caching, find the next unexecuted stage.
#'
#' @name stageRunner_next_stage
#' @return a character stage key giving the next unexecuted stage.
#'   If all stages have been executed, this returns \code{FALSE}.
#'   If the stageRunner does not have caching enabled, this will
#'   always return the first stage key (`'1'`).
stageRunner_next_stage <- function() {
  for (stage_index in seq_along(self$stages)) {
    is_unexecuted_terminal_node <- is.stageRunnerNode(self$stages[[stage_index]]) &&
      !self$stages[[stage_index]]$was_executed()
    has_unexecuted_terminal_node <- is.stagerunner(self$stages[[stage_index]]) &&
      is.character(tmp <- self$stages[[stage_index]]$next_stage())

    if (is_unexecuted_terminal_node) return(as.character(stage_index))
    else if (has_unexecuted_terminal_node)
      return(paste(c(stage_index, tmp), collapse = '/'))
  }
  FALSE
}

