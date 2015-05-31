#' Wrap a function around a stageRunner's terminal nodes
#'
#' If we want to execute some behavior just before and just after executing
#' terminal nodes in a stageRunner, a solution without this method would be
#' to overlay two runners -- one before and one after. However, this is messy,
#' so this function is intended to replace this approach with just one function.
#'
#' Consider the runner
#'   \code{sr <- stageRunner$new(some_env, list(a = function(e) print('2'))}
#' If we run 
#'   \code{sr2 <- stageRunner$new(some_env, list(a = function(e) {
#'     print('1'); yield(); print('3') }))
#'    sr1$around(sr2)
#'    sr1$run()
#'  }
#' then we will see 1, 2, and 3 printed in succession. The \code{yield()}
#' keyword is used to specify when to execute the terminal node that
#' is sandwiched in the "around" runner.
#'
#' @name stageRunner_around
#' @param other_runner stageRunner. Another stageRunner from which to create
#'   an around procedure. Alternatively, we could give a function or a list
#'   of functions.
stageRunner_around <- function(other_runner) {
  if (is.null(other_runner)) return(self)
  if (!is.stagerunner(other_runner)) {
    other_runner <- stageRunner$new(self$.context, other_runner)
  }

  stagenames <- names(other_runner$stages) %||% rep("", length(other_runner$stages))
  lapply(seq_along(other_runner$stages), function(stage_index) {
    name <- stagenames[stage_index]
    this_index <- 
      if (identical(name, "")) stage_index
      else if (is.element(name, names(self$stages))) name
      else return()

    if (is.stagerunner(self$stages[[this_index]]) &&
        is.stagerunner(other_runner$stages[[stage_index]])) {
      self$stages[[this_index]]$around(other_runner$stages[[stage_index]])
    } else if (is.stageRunnerNode(self$stages[[this_index]]) &&
               is.stageRunnerNode(other_runner$stages[[stage_index]])) {
      self$stages[[this_index]]$around(other_runner$stages[[stage_index]])
    } else {
      warning("Cannot apply around stageRunner because ",
              this_index, " is not a terminal node.")
    }
  })
  self
}
