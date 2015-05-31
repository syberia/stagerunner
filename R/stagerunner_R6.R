#' @include stagerunner-initialize.R stagerunner-run.R stagerunner-around.R
#'   stagerunner-coalesce.R stagerunner-overlay.R stagerunner-transform.R
#'   stagerunner-append.R stagerunner-stage_names.R stagerunner-next_stage.R
NULL

#' Generic for printing stageRunner objects.
#' 
#' @name stageRunner_show
#' @param indent integer. Internal parameter for keeping track of nested
#'   indentation level.
stageRunner_show <- function(indent = 0) {
  if (missing(indent)) {
    sum_stages <- function(x) sum(vapply(x,
      function(x) if (is.stagerunner(x)) sum_stages(x$stages) else 1L, integer(1)))
    caching <- if (self$remember) ' caching' else ''
    cat("A", caching, " stageRunner with ", sum_stages(self$stages), " stages:\n", sep = '')
  }
  stage_names <- names(self$stages) %||% rep("", length(self$stages))

  # A helper function for determining if a stage has been run yet.
  began_stage <- function(stage)
    if (is.stagerunner(stage)) any(vapply(stage$stages, began_stage, logical(1)))
    else if (is.stageRunnerNode(stage)) !is.null(stage$.cached_env)
    else FALSE

  lapply(seq_along(stage_names), function(index) {
    prefix <- paste0(rep('  ', (if (is.numeric(indent)) indent else 0) + 1), collapse = '')
    marker <-
      if (self$remember && began_stage(self$stages[[index]])) {
        next_stage <- treeSkeleton$new(self$stages[[index]])$last_leaf()$successor()$object
        if (( is.null(next_stage) && !self$.root()$.finished) ||
            (!is.null(next_stage) && !began_stage(next_stage))) 
          '*' # Use a * if this is the next stage to be executed
          # TODO: Fix the bug where we are unable to tell if the last stage
          # finished without a .finished internal field.
          # We need to look at and set predecessors, not successors.
        else '+' # Other use a + for completely executed stage
      } else '-'
    prefix <- gsub('.$', marker, prefix)
    stage_name <- 
      if (is.na(stage_names[[index]]) || stage_names[[index]] == "")
        paste0("< Unnamed (stage ", index, ") >")
      else stage_names[[index]]
    cat(prefix, stage_name, "\n")
    if (is.stagerunner(self$stages[[index]]))
      self$stages[[index]]$show(indent = indent + 1)
  })

  if (missing(indent)) { cat('Context '); print(self$.context) }
  NULL
}

#' Whether or not the stageRunner has a key matching this input.
#'
#' @param key ANY. The potential key.
#' @return \code{TRUE} or \code{FALSE} accordingly.
stageRunner_has_key <- function(key) {
  has <- tryCatch(normalize_stage_keys(key, self$stages), error = function(.) FALSE)
  any(c(has, recursive = TRUE))
}

#' Clear all caches in this stageRunner, and recursively.
#' @name stageRunner_.clear_cache
stageRunner_.clear_cache <- function() {
  for (i in seq_along(self$stages)) {
    if (is.stagerunner(self$stages[[i]])) self$stages[[i]]$.clear_cache()
    else self$stages[[i]]$.cached_env <- NULL
  }
  TRUE
}

#' Set all parents for this stageRunner, and recursively
#' @name stageRunner_.set_parents
stageRunner_.set_parents <- function() {
  for (i in seq_along(self$stages)) {
    # Set convenience helper attribute "child_index" to ensure that treeSkeleton
    # can find this stage.
    attr(self$stages[[i]], 'child_index') <<- i
    attr(self$stages[[i]], 'parent') <<- self
  }
  self$.parent <- NULL
}

#' Get an environment representing the context directly before executing a given stage.
#'
#' @note If there is a lot of data in the remembered environment, this function
#'   may be computationally expensive as it has to create a new environment
#'   with a copy of all the relevant data.
#' @param stage_index integer. The substage for which to grab the before
#'   environment.
#' @return a fresh new environment representing what would have been in
#'   the context as of right before the execution of that substage.
stageRunner_.before_env <- function(stage_index) {
  cannot_run_error <- function() {
    stop("Cannot run this stage yet because some previous stages have ",
         "not been executed.")
  }

  if (self$with_tracked_environment()) {
    # We are using the objectdiff package and its tracked_environment,
    # so we have to "roll back" to a previous commit.
    current_commit <- paste0(self$.prefix, stage_index)

    if (!current_commit %in% names(package_function("objectdiff", "commits")(self$.context))) {
      if (`first_commit?`(current_commit)) {
        # TODO: (RK) Do this more robustly. This will fail if there is a 
        # first sub-stageRunner with an empty list as its stages.
        package_function("objectdiff", "commit")(self$.context, current_commit)
      } else {
        cannot_run_error()
      }
    } else {
      package_function("objectdiff", "force_push")(self$.context, current_commit)
    }

    env <- new.env(parent = package_function("objectdiff", "parent.env.tracked_environment")(self$.context))
    copy_env(env, package_function("objectdiff", "environment")(self$.context))
    env
  } else {
    env <- self$stages[[stage_index]]$.cached_env
    if (is.null(env)) { cannot_run_error() }

    # Restart execution from cache, so set context to the cached environment.
    copy_env(self$.context, env)
    env
  }
}

#' Mark a given stage as being finished.
#' 
#' @param stage_index integer. The index of the substage in this stageRunner.
stageRunner_.mark_finished <- function(stage_index) {
  node <- treeSkeleton$new(self$stages[[stage_index]])$successor()

  if (!is.null(node)) { # Prepare a cache for the future!
    if (self$with_tracked_environment()) {
      # We assume the head for the tracked_environment is set correctly.
      package_function("objectdiff", "commit")(self$.context, node$object$index())
    } else {
      node$object$.cached_env <- new.env(parent = parent.env(self$.context))
      copy_env(node$object$.cached_env, self$.context)
    }
  } else {
    # TODO: Remove this hack used for printing
    root <- self$.root()
    root$.finished <- TRUE
  }
}

#' Determine the root of the stageRunner.
#'
#' @name stageRunner_.root
#' @return the root of the stageRunner
stageRunner_.root <- function() {
  treeSkeleton$new(self)$root()$object
}

#' @export
print.stageRunner <- function(x, ...) {
  x$show(...)
}

#' @export
print.stageRunnerNode <- function(x, ...) {
  x$show(...)
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



