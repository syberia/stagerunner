#' Wrap a stageRunnerNode callable with another callable.
#'
#' @param other_node stagerunner or stageRunnerNode.
#' @return \code{TRUE} or \code{FALSE} according as the wrapping was
#'    successful.
#' @examples \dontrun{
#' node1 <- stageRunnerNode(function(e) print(2))
#' node2 <- stageRunnerNode(function(e) { print(1); yield(); print(3); })
#' node1$around(node2)
#' node1$run() # Will print 1 2 3
#' # Notice the provided "yield" keyword, which allows calling the
#' # node that is being wrapped.
#' }
stageRunnerNode_around <- function(other_node) {
  if (is.stageRunnerNode(other_node)) other_node <- other_node$callable
  if (is.null(other_node)) return(FALSE)
  if (!is.function(other_node)) {
    warning("Cannot apply stageRunner$around in a terminal ",
            "node except with a function. Instead, I got a ",
            class(other_node)[1])
    return(FALSE)
  }

  new_callable <- other_node

  ## We inject the `yield` keyword.
  environment(new_callable) <- list2env(parent = environment(new_callable), list(
    .parent_context = self,
    yield           = around_yield(self$callable)
  ))
  self$callable <- new_callable
  TRUE
}

around_yield <- function(callable) {
  ## Constructing the yield keyword is a little bit messy. We want to pass
  ## the exact same parameters as the call to the original callable, so
  ## can grab `...` from two frames up. However, since we must also
  ## provide the function we are invoking with `yield` (i.e., the `callable)`,
  ## we have in effect two different kinds of injections.
  yield <- function() {
    # ... lives up two frames, but the run function lives up 1,
    # so we have to do something ugly
    run <- eval.parent(quote(.parent_context$run))
    args <- append(eval.parent(quote(list(...)), n = 2),
      list(.callable = callable))
    do.call(run, args, envir = parent.frame())
  }
  ## We don't need anything except the base environment for the body of
  ## the `yield` keyword itself.
  environment(yield) <- list2env(list(callable = callable), parent = baseenv())
  yield
}

