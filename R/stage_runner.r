# Dynamically create an accessor method for reference classes.
accessor_method <- function(attr) {
  fn <- eval(bquote(
    function(`*VALUE*` = NULL)
      if (missing(`*VALUE*`)) .(substitute(attr))
      else .(substitute(attr)) <<- `*VALUE*`
  ))
  environment(fn) <- parent.frame()
  fn
}

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
#' @param .stages a list. The functions to execute on the \code{context}.
#' @param remember a logical. Whether to keep a copy of the context and its
#'    contents throughout each stage for debugging purposes--this makes it
#'    easy to go back and investigate a stage. This could be optimized by
#'    developing a package for "diffing" two environments. The default is
#'    \code{FALSE}. When set to \code{TRUE}, the return value of the
#'    \code{run} method will be a list of two environments: one of what
#'    the context looked like before the \code{run} call, and another
#'    of the aftermath.
stageRunner__initialize <- function(context, .stages, remember = FALSE, .recursive = TRUE) {
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
    else if (is.function(stages[[i]]))
      stages[[i]] <<- stageRunnerNode$new(stages[[i]], context, .recursive = .recursive)

  # Do not allow the '/' character in stage names, as it's reserved for
  # referencing nested stages.
  if (any(violators <- grepl('/', names(stages), fixed = TRUE))) {
    msg <- paste0("Stage names may not have a '/' character. The following do not ",
      "satisfy this constraint: '",
      paste0(names(stages)[violators], collapse = "', '"), "'")
    stop(msg)
  }

  remember <<- remember
  if (remember) {
    # Set up parents for treeSkeleton.
    .self$.clear_cache()
    .self$.set_parents()

    # Set the first cache environment
    if (length(stages) > 0) {
      first_env <- treeSkeleton$new(stages[[1]])$first_leaf()$object
      first_env$cached_env <- new.env(parent = parent.env(context))
      copy_env(first_env$cached_env, context)
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
#' @param remember_flag logical. An internal argument used by \code{run}
#'   recursively if the \code{stageRunner} object has the \code{remember}
#'   field set to \code{TRUE}. If \code{remember_flag} is FALSE, \code{run}
#'   will not attempt to restore the context from cache (e.g., if we are
#'   executing five stages simultaneously with \code{remember = TRUE},
#'   the first stage's context should be restored from cache but none
#'   of the remaining stages should).
#' @param verbose logical. Whether or not to display pretty colored text
#'   informing about stage progress.
stageRunner__run <- function(stage_key = NULL, to = NULL,
                             normalized = FALSE, verbose = FALSE,
                             remember_flag = TRUE) {
  if (identical(normalized, FALSE))
    stage_key <- normalize_stage_keys(stage_key, stages, to = to)

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
    # terminal node or nested stagerunner. We compute this first
    # in case we run into referencing errors (e.g., the requested
    # stage does not exist).
    run_stage <-
      if (identical(stage_key[[stage_index]], TRUE)) {
        stage <- stages[[stage_index]]
        if (is.stagerunner(stage)) function(...) stage$run(...)
        else if (is.stageRunnerNode(stage)) {
          nested_run <- FALSE
          function(...) stage$run(context)
        }
        else if (is.function(stage)) {
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
        if (nested_run) run_stage(remember_flag = TRUE)$before
        else { # a leaf / terminal node
          if (is.null(env <- stages[[stage_index]]$cached_env))
            stop("Cannot run this stage yet because some previous stages have ",
                 "not been executed.")

          # Restart execution from cache, so set context to the cached environment.
          copy_env(context, env)
          env
        }, parent.env(environment())) # use parent.env(environment()) because
                                      # this is happening in an lapply
      
      # If terminal node, execute the stage (if it was nested,  it's already been
      # executed in order to recursively fetch the before_env).
      if (!nested_run) run_stage() 
    }
    else if (remember) run_stage(remember_flag = FALSE)
    else run_stage()

    if (remember && !nested_run) {
      # When we're done running a stage (i.e., processing a terminal node),
      # set the cache on the successor node to be the current context
      # (since that node will execute starting with what's in the context now --
      # this also ensures that running that node with a separate call to
      # $run will not bump into a "you haven't executed this stage yet" error).
      node <- treeSkeleton$new(stages[[stage_index]])$successor()
      if (!is.null(node)) # Prepare a cache for the future!
        copy_env(node$object$cached_env <- new.env(parent = parent.env(context)), context)
    }

    if (display_message) show_message(names(stages), stage_index, begin = FALSE)
  })

  if (remember && remember_flag) list(before = before_env, after = context)
  else invisible(TRUE)
}

#' Coalescing a stageRunner object is taking another stageRunner object
#' with similar stage names and replacing the latter's cached environments
#' with the former's.
#'
#' @param other_runner stageRunner. Another stageRunner from which to coalesce.
stageRunner__coalesce <- function(other_runner) {
  # TODO: Should we care about insertion of new stages causing cache wipes?
  # For now it seems like this would just be an annoyance.
  stopifnot(remember)
  stagenames <- names(other_runner$stages) %||% rep("", length(other_runner$stages))
  lapply(seq_along(other_runner$stages), function(stage_index) {
    # TODO: Match by name *OR* index
    if (stagenames[[stage_index]] %in% names(stages)) {
      # If both are stageRunners, try to coalesce our sub-stages.
      if (is.stagerunner(stages[[names(stages)[stage_index]]]) &&
          is.stagerunner(other_runner$stages[[stage_index]])) {
          stages[[names(stages)[stage_index]]]$coalesce(
            other_runner$stages[[stage_index]])
      # If both are not stageRunners, copy the cached_env if and only if
      # the stored function and its environment are identical
      } else if (!is.stagerunner(stages[[names(stages)[stage_index]]]) &&
          !is.stagerunner(other_runner$stages[[stage_index]]) &&
          !is.null(other_runner$stages[[stage_index]]$cached_env) #&&
          #identical(deparse(stages[[names(stages)[stage_index]]]$fn),
          #          deparse(other_runner$stages[[stage_index]]$fn)) # &&
          # This is way too tricky and far beyond my abilities..
          #identical(stagerunner:::as.list.environment(environment(stages[[names(stages)[stage_index]]]$fn)),
          #          stagerunner:::as.list.environment(environment(other_runner$stages[[stage_index]]$fn)))
          ) {
        stages[[names(stages)[stage_index]]]$cached_env <<-
          new.env(parent = parent.env(context))
        copy_env(stages[[names(stages)[stage_index]]]$cached_env,
                 other_runner$stages[[stage_index]]$cached_env)
      }
    }
  })
  .self
}

#' Overlaying a stageRunner object is taking another stageRunner object
#' with similar stage names and adding the latter's stages as terminal stages
#' to the former (for example, to support tests).
#'
#' @param other_runner stageRunner. Another stageRunner from which to overlay.
stageRunner__overlay <- function(other_runner) {
  for (stage_index in seq_along(other_runner)) {
    name <- names(other_runner)[[stage_index]]
    index <-
      if (identical(name, '') || identical(name, NULL)) stage_index
      else if (name %in% names(stages)) name
      else stop('Cannot overlay because keys do not match')
    stages[[index]]$overlay(other_runner$stages[[stage_index]])
  }
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

#' Generic for printing stageRunner objects.
#' @param indent integer. Internal parameter for keeping track of nested
#'   indentation level.
stageRunner__show <- function(indent = 0) {
  if (missing(indent)) {
    sum_stages <- function(x) sum(vapply(x,
      function(x) if (is.stagerunner(x)) sum_stages(x$stages) else 1L, integer(1)))
    cat("A stageRunner with", sum_stages(.self$stages), "stages:\n")
  }

  stage_names <- names(stages) %||% rep("", length(stages))
  lapply(seq_along(stage_names), function(index) {
    prefix <- paste0(rep('  ', indent + 1), collapse = '')
    prefix <- gsub('.$', '-', prefix)
    stage_name <- 
      if (is.na(stage_names[[index]]) || stage_names[[index]] == "")
        paste0("< Unnamed (stage ", index, ") >")
      else stage_names[[index]]
    cat(prefix, stage_name, "\n")
    if (is.stagerunner(stages[[stage_name]]))
      stages[[stage_name]]$show(indent = indent + 1)
  })
}

#' Clear all caches in this stageRunner, and recursively.
stageRunner__.clear_cache <- function() {
  for (i in seq_along(stages)) {
    if (is.stagerunner(stages[[i]])) stages[[i]]$.clear_cache()
    else stages[[i]]$cached_env <<- NULL
  }
  TRUE
}

#' Set all parents for this stageRunner, and recursively
stageRunner__.set_parents <- function() {
  for (i in seq_along(stages)) {
    # Set convenience helper attribute "child_index" to ensure that treeSkeleton
    # can find this stage.
    if (inherits(stages[[i]], 'refClass')) {
      # http://stackoverflow.com/questions/22752021/why-is-r-capricious-in-its-use-of-attributes-on-reference-class-objects
      unlockBinding('.self', attr(stages[[i]], '.xData'))
      attr(attr(stages[[i]], '.xData')$.self, 'child_index') <<- i
      lockBinding('.self', attr(stages[[i]], '.xData'))
    } else attr(stages[[i]], 'child_index') <<- i

    if (!inherits(stages[[i]], 'refClass')) {
      attr(stages[[i]], 'parent') <<- .self
    } else {
      # if stages[[i]] has a .set_parents method (e.g. it is a stagerunner), run that
      if ('.set_parents' %in% ls(stages[[i]]$.refClassDef@refMethods, all.names = TRUE))
        stages[[i]]$.set_parents()
      stages[[i]]$parent(.self)
    }
  }
  .parent <<- NULL
}

#' Stage runner is a reference class for parametrizing and executing
#' a linear sequence of actions.
#' 
#' @docType class 
#' @export
#' @name stageRunner
stageRunner <- setRefClass('stageRunner',
  fields = list(context = 'environment', stages = 'list', remember = 'logical',
                .parent = 'ANY'),
  methods = list(
    initialize   = stageRunner__initialize,
    run          = stageRunner__run,
    coalesce     = stageRunner__coalesce,
    stage_names  = stageRunner__stage_names,
    parent       = accessor_method(.parent),
    children     = function() stages,
    show         = stageRunner__show,
    .set_parents = stageRunner__.set_parents,
    .clear_cache = stageRunner__.clear_cache 
  )
)

#' @name stageRunner
#' @export
NULL

#' Check whether an R object is a stageRunner object
#'
#' @export
#' @param obj any object.
#' @return \code{TRUE} if the object is of class
#'    \code{stageRunner}, \code{FALSE} otherwise.
is.stagerunner <- function(obj) inherits(obj, 'stageRunner')

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
#stageRunnerNode <- function(fn, parent_obj, parent_env = parent.frame()) {
#  env <- new.env(parent = parent_env)
#  class(env) <- c('stageRunnerNode', class(env))
#  env$fn <- fn
#
#  # Make a stageRunnerNode commensurate with treeSkeleton
#  # parent will be set later
#  if (!missing(parent_obj)) attr(env, 'parent') <- parent_obj
#  attr(env, 'children') <- list()
#
#  env
#}

stageRunnerNode <- setRefClass('stageRunnerNode',
  fields = list(callable = 'ANY',
                cached_env = 'ANY',
                .context = 'ANY',
                .parent = 'ANY'),
  methods = list(
    initialize = function(.callable, .context = NULL, .recursive = TRUE) {
      #stopifnot(is_any(.callable, c('stageRunner', 'function', 'NULL')))
      stopifnot(is_any(.callable, c('function', 'NULL')))
      #if (is.function(.callable) && .recursive) 
      #  .callable <- stageRunner$new(.context, .callable, .recursive = FALSE)
      callable <<- .callable; .context <<- .context
    },
    run = function(...) {
      if (is.stagerunner(callable)) callable$run(...)
      else callable(.context)
    }, 
    overlay = function(other_node) {
      if (is.stageRunnerNode(other_node)) other_node <- other_node$callable
      stopifnot(is.stagerunner(other_node))
      # TODO: Fancier merging here
      callable$stages <<- append(callable$stages, other_node$stages)
    },
    parent   = accessor_method(.parent),
    children = function() list()
  )
)

is.stageRunnerNode <- function(obj) inherits(obj, 'stageRunnerNode')

