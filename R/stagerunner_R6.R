#' @include stagerunner-initialize.R stagerunner-run.R stagerunner-around.R
#'   stagerunner-coalesce.R stagerunner-overlay.R stagerunner-transform.R
#'   stagerunner-append.R stagerunner-stage_names.R stagerunner-next_stage.R
#'   stagerunner-show.R stagerunner-internal.R
NULL

#' Whether or not the stageRunner has a key matching this input.
#'
#' @param key ANY. The potential key.
#' @return \code{TRUE} or \code{FALSE} accordingly.
stageRunner_has_key <- function(key) {
  has <- tryCatch(normalize_stage_keys(key, self$stages), error = function(.) FALSE)
  any(c(has, recursive = TRUE))
}

stageRunner_ <- R6::R6Class('stageRunner',
  active = list(context = function() self$.context),                            
  public = list(
    .context = NULL,
    stages = list(),
    remember = FALSE,
    .mode = "head",
    .parent = NULL,
    .finished = FALSE,
    .prefix = "",
    initialize   = stagerunner_initialize,
    run          = run,
    around       = stageRunner_around,
    coalesce     = stageRunner_coalesce,
    overlay      = stageRunner_overlay,
    transform    = stageRunner_transform,
    append       = stageRunner_append,
    stage_names  = stageRunner_stage_names,
    parent       = function() { self$.parent },
    children     = function() { self$stages },
    next_stage   = stageRunner_next_stage,
    show         = stageRunner_show,
    has_key      = stageRunner_has_key,
    mode         = function() { self$mode },
    .set_parents = stageRunner_.set_parents,
    .clear_cache = stageRunner_.clear_cache,
    .root        = stageRunner_.root,

    # objectdiff intertwined functionality
    .set_prefixes  = stageRunner_.set_prefixes,
    .before_env    = stageRunner_.before_env,
    .mark_finished = stageRunner_.mark_finished,
    with_tracked_environment = function() {
      out <- is(self$context, 'tracked_environment')
      if (out) { requireNamespace("objectdiff", quietly = TRUE) }
      out
    }
  )
)

#' @export
stageRunner <- structure(
  function(...) { stageRunner_$new(...) },
  class = "stageRunner_"
)

#' @export
stagerunner <- stageRunner

#' @export
`$.stageRunner_` <- function(...) {
  stopifnot(identical(..2, "new"))
  ..1
}

#' Stagerunner nodes are environment wrappers around individual stages
#' (i.e. functions) in order to track meta-data (e.g., for caching).
#' 
#' @param fn function. This will be wrapped in an environment.
#' @param parent_obj stageRunner. The enclosing stageRunner object.
#' @param parent_env environment. The parent environment of the created
#'   \code{stageRunnerNode} object. The default is the calling
#'   environment (i.e., \code{parent.frame()}).
#' @return an environment with some additional attributes for
#'   navigating in a tree-like structure.
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


#' Check whether an R object is a stageRunner object
#'
#' @export
#' @param obj any object.
#' @return \code{TRUE} if the object is of class
#'    \code{stageRunner}, \code{FALSE} otherwise.
is.stagerunner <- function(obj) inherits(obj, 'stageRunner')
#' @export
is.stageRunner <- is.stagerunner
is.stageRunnerNode <- function(obj) inherits(obj, 'stageRunnerNode')



