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

    run = function(..., .cached_env = NULL, .callable = self$callable) {
      # TODO: Clean this up by using environment injection utility fn
      correct_cache <- .cached_env %||% self$.cached_env
      if (is.null(.callable)) FALSE
      else if (is.stagerunner(.callable)) {
        .callable$run(..., .cached_env = correct_cache)
      } else {
        tmp <- new.env(parent = environment(.callable))
        environment(.callable) <- tmp
        environment(.callable)$cached_env <- correct_cache
        on.exit(environment(.callable) <- parent.env(environment(.callable)))
        .callable(self$.context, ...)
      }
      self$executed <- TRUE
    }, 

    # This function goes hand in hand with stageRunner$around
    around = function(other_node) {
      if (is.stageRunnerNode(other_node)) other_node <- other_node$callable
      if (is.null(other_node)) return(FALSE)
      if (!is.function(other_node)) {
        warning("Cannot apply stageRunner$around in a terminal ",
                "node except with a function. Instead, I got a ",
                class(other_node)[1])
        return(FALSE)
      }

      new_callable <- other_node
      # Inject yield() keyword
      yield_env <- new.env(parent = environment(new_callable))
      yield_env$.parent_context <- self
      yield_env$yield <- function() {
        # ... lives up two frames, but the run function lives up 1,
        # so we have to do something ugly
        run <- eval.parent(quote(.parent_context$run))
        args <- append(eval.parent(quote(list(...)), n = 2),
          list(.callable = callable))
        do.call(run, args, envir = parent.frame())
      }
      environment(yield_env$yield) <- new.env(parent = baseenv())
      environment(yield_env$yield)$callable <- self$callable

      environment(new_callable) <- yield_env
      self$callable <- new_callable
      TRUE
    },

    overlay = function(other_node, label = NULL, flat = FALSE) {
      if (is.stageRunnerNode(other_node)) other_node <- other_node$callable
      if (is.null(other_node)) return(FALSE)
      if (!is.stagerunner(other_node)) 
        other_node <- stageRunner$new(self$.context, other_node)

      # Coerce the current callable object to a stageRunner so that
      # we can append the other_node's stageRunner.
      if (!is.stagerunner(self$callable)) 
        self$callable <- stageRunner$new(self$.context, self$callable)

      # TODO: Fancier merging here
      if (isTRUE(flat)) {
        if (!is.character(label)) stop("flat coalescing needs a label")
        self$callable$stages[[label]] <- other_node
      } else self$callable$append(other_node, label)
    },
    transform = function(transformation) {
      if (is.stagerunner(self$callable)) self$callable$transform(transformation)
      else self$callable <- transformation(self$callable)
    },
    was_executed = function() { self$executed },
    parent   = function() { attr(self, "parent") }, # accessor_method(.parent),
    children = function() list(),
    show     = function() { cat("A stageRunner node containing: \n"); print(self$callable) },

    # Functions which intertwine with the objectdiff package
    index    = function() {
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

#' @export
#' @rdname stageRunnerNode
is.stageRunnerNode <- function(obj) inherits(obj, 'stageRunnerNode')

