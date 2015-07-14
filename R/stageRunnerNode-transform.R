## This helper method is useful when we want to apply some transformation
## to *all* terminal nodes of a stageRunner. You can think of it as 
## \code{\link{rapply}} for stageRunners.
#' Transform a stageRunnerNode according to a functional.
#'
#' @param transformation function. An arity-1 function which takes the
#'    \code{callable} of a \code{stageRunnerNode} and transforms it
#'    into another callable (i.e. a function or a stagerunner). If the
#'    original \code{callable} is a stagerunner, its terminal nodes in
#'    turn will be transformed recursively.
#' @return The transformed callable.
#' @examples \dontrun{
#' increment <- 1
#' adder     <- function(x) x + increment
#' node      <- stageRunnerNode$new(function(e) print(adder(1)))
#' node$transform(function(fn) {
#'   environment(fn)$increment <- environment(fn)$increment + 1; fn
#' })
#' node$run() # Prints 3, rather than 2
#' }
stageRunnerNode_transform <- function(transformation) {
  if (is.stagerunner(self$callable)) {
    self$callable$transform(transformation)
  } else {
    self$callable <- transformation(self$callable)
  }
}

