## Coalescing two stagerunners is a critical operation for modifying a 
## runner that is "in flight" and is currently being executed.
##
## Imagine we have our usual example runner:
##
##   * __import data__
##   * __clean data__
##      * __impute variable 1__
##      * discretize variable 2
##   * train model
## 
## It has `remember = TRUE`, which means it is keeping a copy of the 
## current context in each stage. As in the example, we have executed it
## to the imputation substage. We now realize there is a mistake in the
## imputation code. We can re-create a new fresh stagerunner with the
## same structure, but it will not have the history of context changes!
##
## Instead, we must *coalesce* the old runner onto the new runner, so that it
## carries over the environment changes. That way, when we continue execution
## from the imputation substage in our fixed runner, it will resume as before
## without having to re-import the data.
##
## This can be inefficient for large datasets, but using the
## [objectdiff](http://github.com/syberia/objectdiff) package we can avoid
## the memory problems that may arise. For even larger datasets, we may need
## database-backed storage, but this is beyond the scope of stagerunners for
## now.
#' Coalescing a stageRunner object is taking another stageRunner object
#' with similar stage names and replacing the latter's cached environments
#' with the former's.
#'
#' @name stageRunner_coalesce
#' @param other_runner stageRunner. Another stageRunner from which to coalesce.
#' @note coalescing is ill-defined for stageRunner with unnamed stages,
#'    since it is impossible to tell when a stage has changed.
stageRunner_coalesce <- function(other_runner) {
  # TODO: Should we care about insertion of new stages causing cache wipes?
  # For now it seems like this would just be an annoyance.
  # stopifnot(remember)
  if (!isTRUE(self$remember)) return()

  ## We must handle these cases: (1) integration with 
  ## [objectdiff](http://github.com/syberia/objectdiff), and (2) vanilla
  ## R environment objects. Both are tricky.
  if (self$with_tracked_environment()) {
    if (!other_runner$with_tracked_environment()) {
      stop("Cannot coalesce stageRunners using tracked_environments with ",
           "those using vanilla environments", call. = FALSE)
    }

    compare_head <- function(x, y) {
      m <- seq_len(min(length(x), length(y)))
      x[m] != y[m]
    }

    common <- sum(cumsum(compare_head(self$stage_names(), other_runner$stage_names())) == 0)
    # Warning: Coalescing stageRunners with tracked_environments does not
    # duplicate the tracked_environment, so the other_runner becomes invalidated,
    # and this is a destructive action.
    # TODO: (RK) What if the tracked_environment given initially to the stageRunner
    # already has some commits?
    commits     <- package_function("objectdiff", "commits")
    `.context<-` <- function(obj, value) {
      if (is.stagerunner(obj)) {
        obj$.context <- value
        for (stage in obj$stages) { Recall(stage, value) }
      } else if (is.stageRunnerNode(obj)) {
        obj$.context <- value
        if (is.stagerunner(obj$callable)) { Recall(obj$callable, value) }
      }
    }
    self$.context  <- other_runner$.context
    for (stage in self$stages) { .context(stage) <- other_runner$.context }
    ## Mark common executed stages.
    self_iterator  <- treeSkeleton(self)$root()$first_leaf()
    other_iterator <- treeSkeleton(other_runner)$root()$first_leaf()
    for (i in seq_along(common)) {
      self_iterator$object$executed <- other_iterator$object$executed
      self_iterator  <- self_iterator$successor()
      other_iterator <- other_iterator$successor()
    }

    ## Coalescing is a destructive action since the other runner will no longer
    ## be able to perform its function after the environments are moved.
    other_runner$.context <- new.env(parent = emptyenv())
    commit_count   <- length(commits(self$.context)) 
    mismatch_count <- commit_count - (common + 1)
    if (mismatch_count > 0) {
      package_function("objectdiff", "force_push")(self$.context, commit_count)
      package_function("objectdiff", "rollback")  (self$.context, mismatch_count)
    }
  } else {
    if (other_runner$with_tracked_environment()) {
      stop("Cannot coalesce stageRunners using vanilla environments with ",
           "those using tracked_environments", call. = FALSE)
    }

    stagenames <- names(other_runner$stages) %||% character(length(other_runner$stages))
    lapply(seq_along(other_runner$stages), function(stage_index) {
      # TODO: Match by name *OR* index
      if (stagenames[[stage_index]] %in% names(self$stages)) {
        # If both are stageRunners, try to coalesce our sub-stages.
        if (is.stagerunner(self$stages[[names(self$stages)[stage_index]]]) &&
            is.stagerunner(other_runner$stages[[stage_index]])) {
            self$stages[[names(self$stages)[stage_index]]]$coalesce(
              other_runner$stages[[stage_index]])
        # If both are not stageRunners, copy the cached_env if and only if
        # the stored function and its environment are identical
        } else if (!is.stagerunner(self$stages[[names(self$stages)[stage_index]]]) &&
            !is.stagerunner(other_runner$stages[[stage_index]]) &&
            !is.null(other_runner$stages[[stage_index]]$.cached_env) #&&
            #identical(deparse(stages[[names(stages)[stage_index]]]$fn),
            #          deparse(other_runner$stages[[stage_index]]$fn)) # &&
            # This is way too tricky and far beyond my abilities..
            #identical(stagerunner:::as.list.environment(environment(stages[[names(stages)[stage_index]]]$fn)),
            #          stagerunner:::as.list.environment(environment(other_runner$stages[[stage_index]]$fn)))
            ) {
          self$stages[[names(self$stages)[stage_index]]]$.cached_env <-
            new.env(parent = parent.env(self$.context))
          if (is.environment(other_runner$stages[[stage_index]]$.cached_env) &&
              is.environment(self$stages[[names(self$stages)[stage_index]]]$.cached_env)) {
            copy_env(self$stages[[names(self$stages)[stage_index]]]$.cached_env,
                     other_runner$stages[[stage_index]]$.cached_env)
            self$stages[[names(self$stages)[stage_index]]]$executed <- 
              other_runner$stages[[stage_index]]$executed
          }
        }
      }
    })
    self$.set_parents()
  }
  self
}


