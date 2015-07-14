## In order to give us more flexibility on the terminal nodes of a
## stagerunner (the actual functions that will be executed on the
## stagerunner's `context`), we wrap them in an R6 class called a
## `[stageRunnerNode]`. This will be extremely useful if we wish to
## dynamically allow our stagerunners to be extended or wrapped with
## functionality.
##
## For example, if we have a runner such as
##
##    1. Import data.
##    2. Munge data.
##    3. Create model.
##    4. Export model.
##
## we might want to run tests for each stage. To do so, we can replace
## each terminal node, a function, with a stagerunner consisting of
## two functions. We can do this with the `overlay` helper method:
##
## `runner$overlay(test_runner)`
##
## Here, `test_runner` is another stagerunner with the exact same
## structure as our main runner, but with testing functions in its
## terminal nodes.
#' Stagerunner nodes are environment wrappers around individual stages
#' (i.e. functions) in order to track meta-data (e.g., for caching).
#' 
#' @name stageRunnerNode
#' @format NULL
#' @docType class
stageRunnerNode_ <- R6::R6Class('stageRunnerNode',
  public = list(
    callable = NULL,
    .cached_env = NULL,
    .context = NULL,
    .parent = NULL,
    executed = FALSE,
    initialize = function(.callable, .context = NULL) {
      stopifnot(is_any(.callable, c('stageRunner', 'function', 'NULL')))
      self$callable <- .callable
      self$.context <- .context
      self$executed <- FALSE
    },

    run          = stageRunnerNode_run,
    around       = stageRunnerNode_around,
    overlay      = stageRunnerNode_overlay,
    transform    = stageRunnerNode_transform,
    was_executed = function() { self$executed },
    parent       = function() { attr(self, "parent") },
    children     = function() list(),
    show         = function() { cat("A stageRunner node containing: \n"); print(self$callable) },

    # Functions which intertwine with the objectdiff package
    index        = function() {
      ix <- which(vapply(attr(self, "parent")$stages,
        function(x) identical(self, x), logical(1)))
      paste0(attr(self, "parent")$.prefix, ix)
    }
  )
)

#' @export
stageRunnerNode <- structure(
  function(...) { stageRunnerNode_$new(...) },
  class = "stageRunnerNode_"
)

#' @export
`$.stageRunnerNode_` <- function(...) {
  stopifnot(identical(..2, "new"))
  ..1
}

#' @param obj ANY. An object to test for class \code{stageRunnerNode}.
#' @export
#' @rdname stageRunnerNode
is.stageRunnerNode <- function(obj) inherits(obj, 'stageRunnerNode')

