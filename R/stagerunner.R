#' @include stagerunner-initialize.R stagerunner-run.R stagerunner-around.R
#'   stagerunner-coalesce.R stagerunner-overlay.R stagerunner-transform.R
#'   stagerunner-append.R stagerunner-stage_names.R stagerunner-current_stage.R
#'   stagerunner-next_stage.R stagerunner-show.R stagerunner-has_key.R
#'   stagerunner-internal.R
#'   stageRunnerNode.R
NULL

## We use [R6](https://github.com/wch/R6) instead of the built-in 
## [reference classes](https://stat.ethz.ch/R-manual/R-devel/library/methods/html/refClass.html) 
## for several reasons.
##
##    1. Their definition is much more compact.
##    2. It is possible to extend R6 definitions cross-packages.
##    3. They suppor the notion of public and private membership.
##
## A stagerunner is clearly represented as a reference object, rather than an
## S3 or S4 class, as it is by nature highly mutable: every stage execution
## triggers updates of the corresponding stage caches.
##
## A stagerunner is primarly defined by its **context** and its **stages**.
## The former is an environment (or when used in conjunction with
## [objectdiff](https://github.com/syberia/objectdiff), a 
## \code{\link[objectdiff]{tracked_environment}}
## that holds the current state of the stagerunner.
##
## A stagerunner's stages are a nested list of either `stageRunnerNode`s
## (wrappers for functions) or more stagerunners, the latter if we wish to
## group together logically bound collections of functions (like a data
## preparation procedure or a sequence of modeling steps).
#' Stagerunners are parametrized sequences of linear execution.
#' 
#' @name stageRunner
#' @format NULL
#' @docType class
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
    initialize     = stagerunner_initialize,
    run            = run,
    around         = stageRunner_around,
    coalesce       = stageRunner_coalesce,
    overlay        = stageRunner_overlay,
    transform      = stageRunner_transform,
    append         = stageRunner_append,
    stage_names    = stageRunner_stage_names,
    parent         = function() { self$.parent },
    children       = function() { self$stages },
    current_stage  = stageRunner_current_stage,
    next_stage     = stageRunner_next_stage,
    show           = stageRunner_show,
    has_key        = stageRunner_has_key,
    mode           = function() { self$mode },
    .set_parents   = stageRunner_.set_parents,
    .clear_cache   = stageRunner_.clear_cache,
    .root          = stageRunner_.root,

    # objectdiff intertwined functionality
    .set_prefixes  = stageRunner_.set_prefixes,
    .before_env    = stageRunner_.before_env,
    .mark_finished = stageRunner_.mark_finished,
    with_tracked_environment = function() {
      out <- is(self$context, 'tracked_environment')
      if (isTRUE(out)) { requireNamespace("objectdiff", quietly = TRUE) }
      out
    }
  )
)

## A little trick to ensure that a stagerunner can be constructed both as
## `stagerunner(...) and stagerunner$new(...)`.
#' @rdname stageRunner
#' @param ... Arguments to pass to stagerunner initialization.
#' @export
stageRunner <- structure(
  function(...) { stageRunner_$new(...) },
  class = "stageRunner_"
)

#' @export
#' @rdname stageRunner
stagerunner <- stageRunner

## To make the above trick work, we need to prevent access to everything except
## `new`.
#' @export
`$.stageRunner_` <- function(...) {
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
#' @rdname is.stagerunner
#' @export
is.stageRunner <- is.stagerunner

