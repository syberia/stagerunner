#' @include stagerunner-initialize.R stagerunner-run.R stagerunner-around.R
#'   stagerunner-coalesce.R stagerunner-overlay.R stagerunner-transform.R
#'   stagerunner-append.R stagerunner-stage_names.R stagerunner-next_stage.R
#'   stagerunner-show.R stagerunner-has_key.R stagerunner-internal.R
#'   stageRunnerNode.R
NULL

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

#' Check whether an R object is a stageRunner object
#'
#' @export
#' @param obj any object.
#' @return \code{TRUE} if the object is of class
#'    \code{stageRunner}, \code{FALSE} otherwise.
is.stagerunner <- function(obj) inherits(obj, 'stageRunner')
#' @export
is.stageRunner <- is.stagerunner

