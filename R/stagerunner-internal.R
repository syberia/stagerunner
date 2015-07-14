## This file contains some messy internal methods that are necessary
## for correct interoperation with the `treeSkeleton` class and
## the [objectdiff](http://github.com/robertzk/objectdiff) package.
#' Clear all caches in this stageRunner, and recursively.
#' @name stageRunner_.clear_cache
stageRunner_.clear_cache <- function() {
  for (i in seq_along(self$stages)) {
    ## The stagerunner context just prior to stage execution is stored
    ## in an environment cache. We clear this cache recursively.
    if (is.stagerunner(self$stages[[i]])) self$stages[[i]]$.clear_cache()
    else self$stages[[i]]$.cached_env <- NULL
  }
  TRUE
}

## The `treeSkeleton` requires a recursive structure to be annotated with
## "parent metadata" so it can be traversed like a tree structure. This is
## what allows us to go from stage "2/2" to stage "3", for example: we
## are finding the successor node in the tree structure and "running" it.
#' Set all parents for this stageRunner, and recursively
#' @name stageRunner_.set_parents
stageRunner_.set_parents <- function() {
  for (i in seq_along(self$stages)) {
    # Set convenience helper attribute "child_index" to ensure that treeSkeleton
    # can find this stage.
    ## The metadata required by the `treeSkeleton` class.
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

