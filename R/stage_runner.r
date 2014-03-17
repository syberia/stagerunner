#' Initialize a stageRunner object.
#'
#' stageRunner objects are used for executing a linear sequence of
#' actions on a context (an environment). For example, if we have an
#' environment \code{e} containing \code{x = 1, y = 2}, then using
#' \code{stages = list(function(e) e$x <- e$x + 1, function(e) e$y <- e$y - e$x)}
#' will cause \code{x = 2, y = 0} after running the stages.
#'
#' @param context an environment. The initial environment that is getting
#'    modified during the execution of the stages. 
#' @param stages a list. The functions to execute on the \code{context}.
#' @param remember a logical. Whether to keep a copy of the context and its
#'    contents throughout each stage for debugging purposes--this makes it
#'    easy to go back and investigate a stage. This could be optimized by
#'    developing a package for "diffing" two environments. The default is
#'    \code{FALSE}.
stageRunner__initialize <- function(context, .stages, remember = FALSE) {
  context <<- context

  legal_types <- function(x) is.function(x) || all(vapply(x,
    function(s) is.function(s) || is.stagerunner(s) ||
      (is.list(s) && legal_types(s)), logical(1)))
  stopifnot(legal_types(.stages))
  if (is.function(.stages)) .stages <- list(.stages)
  stages <<- .stages

  # Construct recursive stagerunners out of a list of lists.
  for (i in seq_along(stages))
    if (is.list(stages[[i]]))
      stages[[i]] <<- stageRunner$new(context, stages[[i]], remember = remember)

  # Do not allow the '/' character in stage names, as it's reserved for
  # referencing nested stages.
  if (any(violators <- grepl('/', names(stages)))) {
    msg <- paste0("Stage names may not have a '/' character. The following do not ",
      "satisfy this constraint: '",
      paste0(names(stages)[violators], collapse = "', '"), "'")
    stop(msg)
  }

  remember <<- remember
  if (remember) {
    stageRunner__.clear_cache(init = TRUE)
    stageRunner__.set_parents(init = TRUE)

    if (length(.environment_cache) > 0) {
      prev_stage <- NULL
      stage <- stages[[1]]
      while (is.stagerunner(stage)) {
        prev_stage <- stage
        stage <- stage$stages[[1]]
      }
      if (is.null(prev_stage)) {
        .environment_cache[[1]] <<- new.env(parent = parent.env(context))
        copy_env(.environment_cache[[1]], context)
      } else {
        prev_stage$.environment_cache[[1]] <- new.env(parent = parent.env(context))
        copy_env(prev_stage$.environment_cache[[1]], context)
      }
    }
  }
}

#' Run the stages in a stageRunner object.
#'
#' @param stage_key an indexing parameter. Many forms are accepted, but the
#'   easiest is the name of the stage. For example, if we have
#'   \code{stageRunner$new(context, list(stage_one = some_fn, stage_two = some_other_fn))}
#'   then using \code{run('stage_one')} will execute \code{some_fn}.
#'   Additional indexing forms are logical (which stages to execute),
#'   numeric (which stages to execute by indices), negative (all but the
#'   given stages), character (as above), and nested forms of these.
#'   The latter refers to instances of the following:
#'   \code{stageRunner$new(context, list(stage_one =
#'     stageRunner$new(context, substage_one = some_fn, substage_two = other_fn),
#'     stage_two = another_fn))}.
#'   Here, the following all execute only substage_two:
#'   \code{run(list(list(FALSE, TRUE), FALSE))},
#'   \code{run(list(list(1, 2)))},
#'   \code{run('stage_one/substage_two')},
#'   \code{run('one/two')},
#'   \code{run(list(list('one', 'two')))},
#'   \code{run(list(list('one', 2)))}
#'   Notice that regular expressions are allowed for characters.
#'   The default is \code{NULL}, which runs the whole sequences of stages.
#' @param to an indexing parameter. If \code{stage_key} refers to a single stage,
#'   attempt to run from that stage to this stage (or, if this one comes first,
#'   this stage to that stage). For example, if we have
#'      \code{stages = list(a = list(b = 1, c = 2), d = 3, e = list(f = 4, g = 5))}
#'   where the numbers are some functions, and we call \code{run} with
#'   \code{stage_key = 'a/c'} and \code{to = 'e/f'}, then we would execute
#'   stages \code{"a/c", "d", "e/f"}.
#' @param normalized logical. A convenience recursion performance helper. If
#'   \code{TRUE}, stageRunner will assume the \code{stage_key} argument is a
#'   nested list of logicals.
#' @param verbose logical. Whether or not to display pretty colored text
#'   informing about stage progress.
stageRunner__run <- function(stage_key = NULL, to = NULL,
                             normalized = FALSE, verbose = FALSE,
                             remember_flag = TRUE) {
  if (identical(normalized, FALSE))
    stage_key <- normalize_stage_keys(stage_key, stages)

  if (!missing(to)) {
    to_key <- normalize_stage_keys(to, stages)
    if (!compare_stage_keys(stage_key, to_key)) {
      # stage_key occurs after to_key
      tmp <- stage_key
      stage_key <- to_key
      to_key <- tmp
    }
    # to go from the first key to the last, populate all the entries
    # after the first TRUE w/ TRUE in stage_key, and all the entries
    # before the first TRUE w/ TRUE in to_key, and then intersect.
    stage_key <- special_and_lists(
      boolean_fill(stage_key, forward = TRUE),
      boolean_fill(to_key, forward = FALSE)
    )
  }

  # Now that we have determined which stages to run, cycle through them all.
  # It is up to the user to determine that context changes make sense.
  # We also implicitly sort the stages to ensure linearity is preserved.
  # Stagerunner enforces the linearity and directionality set in the stage definitions.
  
  # If we are remembering changes, recall what the environment looked like
  # *before* we ran anything.
  before_env <- NULL

  lapply(seq_along(stage_key), function(stage_index) {
    display_message <- verbose && contains_true(stage_key[[stage_index]])
    if (display_message) show_message(names(stages), stage_index, begin = TRUE)

    nested_run <- TRUE
    
    # Determine how to run this stage, depending on whether it is an
    # atomic function or nested stagerunner. We compute this first
    # in case we run into referencing errors (e.g., the requested
    # stage does not exist).
    run_stage <-
      if (identical(stage_key[[stage_index]], TRUE)) {
        stage <- stages[[stage_index]]
        if (is.stagerunner(stage)) function(...) stage$run(...)
        else {
          nested_run <- FALSE
          function(...) stage(context)
        }
      } else if (is.list(stage_key[[stage_index]])) {
        if (!is.stagerunner(stages[[stage_index]]))
          stop("Invalid stage key: attempted to make a nested stage reference ",
               "to a non-existent stage")
        function(...) stages[[stage_index]]$run(stage_key[[stage_index]], normalized = TRUE, ...)
      } else return(FALSE)

    # Now handle when remember = TRUE, i.e., we have to cache the
    # progress along each stage.
    if (remember && remember_flag && is.null(before_env)) {
      # If remember = remember_flag = TRUE and before_env has not been set
      # this is the first stage of a $run() call, so use the cached
      # environment.
      assign('before_env',
        #if (!remember_flag) TRUE
        if (nested_run) run_stage(remember_flag = TRUE)
        else {
          if (is.null(.environment_cache[[stage_index]]))
            stop("Cannot run this stage yet because some previous stages have ",
                 "not been executed.")

          # Restart execution from cache, so set context to the cached environment.
          copy_env(context, .environment_cache[[stage_index]])
          .environment_cache[[stage_index]]
        }, parent.env(environment()))
      
      # If atomic function, execute the stage (if it was nested,
      # it's already been executed in order to recursively fetch the before_env
      if (!nested_run) {
        run_stage() 
      
        # Get next cache ready
        # TODO: Document / comment the shit out of this!
        #if (stage_index + 1 <= length(stages)) {
        #  set_first_cache(stages[[stage_index + 1]], context, stage_index + 1)
        #} else {
        #  set_first_cache(.parent, context)
        #}
      }
    } else if (remember) {
      # If not a nested run, we should cache the environment (no need to cache
      # for nested runs since it'll be cached downstairs -- i.e., only use
      # a cache in the leaves/terminal nodes of a stagerunner).
      if (!nested_run) {
        if (is.null(.environment_cache[[stage_index]]))
          .environment_cache[[stage_index]] <<-
            new.env(parent = parent.env(context))
        copy_env(.environment_cache[[stage_index]], context)
      }

      # When running nested stages, we no longer need to return a before_env
      run_stage(remember_flag = FALSE)
    } else run_stage()

    if (remember && !nested_run) {
      env <- successor_env(stages[[stage_index]], .self)
      if (!identical(env, FALSE)) {
        # Prepare a cache for the future!
        copy_env(env, context)
      }
    }

    if (display_message) show_message(names(stages), stage_index, begin = FALSE)
  })

  if (remember && remember_flag) before_env else invisible(TRUE)
}

#' Retrieve a flattened list of canonical stage names for a stageRunner object
#'
#' For example, if we have stages
#'   \code{stages = list(a = list(b = 1, c = 2), d = 3, e = list(f = 4, g = 5))}
#' then this method would return
#'   \code{list('a/b', 'a/c', 'd', 'e/f', 'e/g')}
#'
#' @return a list of canonical stage names.
#' @examples
#' f <- function() {}
#' sr <- stageRunner$new(new.env(),
#'   list(a = stageRunner$new(new.env(), list(b = f, c = f)), d = f,
#'   e = stageRunner$new(new.env(), list(f = f, g = f))))
#' sr$stage_names()
stageRunner__stage_names <- function() {
  nested_stages <- function(x) if (is.stagerunner(x)) nested_stages(x$stages) else x
  nested_names(lapply(stages, nested_stages))
}

#' Clear all caches in this stageRunner, and recursively.
stageRunner__.clear_cache <- function(init = FALSE) {
  (if (init) eval.parent else eval)(substitute({
    .environment_cache <<- lapply(seq_along(stages), function(.) NULL)
    
    lapply(stages, function(stage) {
      if (is.stagerunner(stage)) stage$.clear_cache()   
    })
  }))

  TRUE
}

#' Set all parents for this stageRunner, and recursively
stageRunner__.set_parents <- function(init = FALSE) {
  (if (init) eval.parent else eval)(substitute({
    lapply(stages, function(stage) {
      if (is.stagerunner(stage)) {
        stage$.parent <- .self
        stage$.set_parents()
      }
    })
  }))

  TRUE
}

#' Stage runner is a reference class for parametrizing and executing
#' a linear sequence of actions.
#' 
#' @docType class 
#' @export
#' @name stageRunner
stageRunner <- setRefClass('stageRunner',
  fields = list(context = 'environment', stages = 'list', remember = 'logical',
                .environment_cache = 'list', .parent = 'ANY'),
  methods = list(
    initialize   = stagerunner:::stageRunner__initialize,
    run          = stagerunner:::stageRunner__run,
    stage_names  = stagerunner:::stageRunner__stage_names,
    .clear_cache = stagerunner:::stageRunner__.clear_cache,
    .set_parents = stagerunner:::stageRunner__.set_parents
  )
)

#' @name stageRunner
#' @export
NULL

#' Check whether an R object is a stageRunner object
#'
#' @export
#' @param obj any object. \code{TRUE} if the object is of class
#'    \code{stageRunner}, \code{FALSE} otherwise.
is.stagerunner <- function(obj) inherits(obj, 'stageRunner')

