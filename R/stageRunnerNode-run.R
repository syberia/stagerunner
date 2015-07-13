#' Execute the callable of a stageRunnerNode.
#'
#' @param ... additional arguments to the \code{callable}. This allows,
#'    stagerunner stages to be uniformly parametrized (for example,
#'    if all stages should have a \code{verbose} parameter.
#' @param .cached_env An internal helper that passes the cached environment
#'    to be used for storing the results of this execution.
#' @param .callable Another internal helper used for some recursive
#'    metaprogramming.
#' @return \code{TRUE} if the execution was successful, or an error otherwise.
stageRunnerNode_run <- function(..., .cached_env = NULL, .callable = self$callable) {
  # TODO: Clean this up by using environment injection utility fn
  correct_cache <- .cached_env %||% self$.cached_env
  if (is.null(.callable)) {
    FALSE
  } else if (is.stagerunner(.callable)) {
    .callable$run(..., .cached_env = correct_cache)
  } else {
    ## If we are executing a function, we inject the \code{cached_env}
    ## into the environment for use by, e.g., testing functions. Ideally,
    ## the callable should be able to determine what the state of the runner
    ## looked like before execution.
    environment(.callable) <- list2env(
      list(cached_env = correct_cache), parent = environment(.callable)
    )
    ## But once this function finishes executing, restore the environment of
    ## the callable to its former glory (i.e., remove the `cached_env`).
    on.exit(environment(.callable) <- parent.env(environment(.callable)))
    .callable(self$.context, ...)
  }
  self$executed <- TRUE
}


