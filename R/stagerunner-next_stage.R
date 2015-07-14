## Stagerunners allow for the option to use a special mode called `"next"`. In
## this mode, instead of executing by default from the beginning of the
## stagerunner, execution will commence from the last non-executed stage.
##
## This allows us to repeatedly call `runner$run()` until it has finished
## executing, if errors occur during the process and we repeatedly fix them.
#' For stageRunners with caching, find the next unexecuted stage.
#'
#' @name stageRunner_next_stage
#' @return a character stage key giving the next unexecuted stage.
#'   If all stages have been executed, this returns \code{FALSE}.
#'   If the stageRunner does not have caching enabled, this will
#'   always return the first stage key (`'1'`).
stageRunner_next_stage <- function() {
  for (stage_index in seq_along(self$stages)) {
    ## We use the `stageRunnerNode$was_executed` helper to determine if this
    ## stage has been executed yet.
    is_unexecuted_terminal_node <- is.stageRunnerNode(self$stages[[stage_index]]) &&
      !self$stages[[stage_index]]$was_executed()
    if (is_unexecuted_terminal_node) return(as.character(stage_index))

    ## We can recursively use `next_stage` if the current stage is another
    ## stagerunner rather than a terminal node.
    has_unexecuted_terminal_node <- is.stagerunner(self$stages[[stage_index]]) &&
      is.character(tmp <- self$stages[[stage_index]]$next_stage())

    if (has_unexecuted_terminal_node) return(paste(c(stage_index, tmp), collapse = '/'))
  }
  FALSE
}

