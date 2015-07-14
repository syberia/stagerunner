## Straightforwardly, apply some function (`transformation`)
## to every terminal node in the stagerunner. This is useful for
## simple debugging and monitoring. For example, if we wish to 
## print the variables currently in the context of stagerunner
## prior to executing each stage, we can call
##
## ```r
## runner$transform(function(fn) {
##   function(context, ...) {
##     print(ls(context))
##     fn(context, ...)
##   }
## })
## ```
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
  for (stage_index in seq_along(self$stages)) {
    self$stages[[stage_index]]$transform(transformation)
  }
  self
}


