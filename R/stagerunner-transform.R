#' Transform the callable's of the terminal nodes of a stageRunner.
#'
#' Every terminal node in a stageRunner is of type stageRunnerNode.
#' These each have a callable, and this method transforms those
#' callables in the way given by the first argument.
#'
#' @name stageRunner_transform
#' @param transformation function. The function which transforms one callable
#'   into another.
stageRunner_transform <- function(transformation) {
  for (stage_index in seq_along(self$stages))
    self$stages[[stage_index]]$transform(transformation)
}

