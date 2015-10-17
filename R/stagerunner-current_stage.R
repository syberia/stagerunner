## This allows us get the stage most recently executed.
#'
#' @name stageRunner_current_stage
#' @return a character stage key giving the latest executed stage.
#'   If the stageRunner does not have caching enabled, this will
#'   always return the first stage key (`'1'`).
stageRunner_current_stage <- function() {
  for (stage_index in rev(seq_along(self$stages))) {
    ## We use the `stageRunnerNode$was_executed` helper to determine if this
    ## stage has been executed yet.
    is_executed_terminal_node <- is.stageRunnerNode(self$stages[[stage_index]]) &&
      self$stages[[stage_index]]$was_executed()

    if (is_executed_terminal_node) return(as.character(stage_index))

    ## We can recursively use `current_stage` if the current stage is another
    ## stagerunner rather than a terminal node.
    has_executed_terminal_node <- is.stagerunner(self$stages[[stage_index]]) &&
      is.character(tmp <- self$stages[[stage_index]]$current_stage())

    if (has_executed_terminal_node) return(paste(c(stage_index, tmp), collapse = '/'))
  }
  FALSE
}

