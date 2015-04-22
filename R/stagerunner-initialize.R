#' Initialize a stageRunner object.
#'
#' stageRunner objects are used for executing a linear sequence of
#' actions on a context (an environment). For example, if we have an
#' environment \code{e} containing \code{x = 1, y = 2}, then using
#' \code{stages = list(function(e) e$x <- e$x + 1, function(e) e$y <- e$y - e$x)}
#' will cause \code{x = 2, y = 0} after running the stages.
#'
#' @name stageRunner_initialize
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
#' @param mode character. Controls the default behavior of calling the
#'    \code{run} method for this stageRunner. The two supported options are
#'    "head" and "next". The former gives a stageRunner which always begins
#'    from the first stage if the \code{from} parameter to the \code{run}
#'    method is blank. Otherwise, it will begin from the previous unexecuted
#'    stage.  The default is "head". This argument has no effect if
#'    \code{remember = FALSE}.
stageRunner_initialize <- function(context, .stages, remember = FALSE,
                                    mode = getOption("stagerunner.mode") %||% 'head') {
  # We must do our own type checking on context for compatibility with
  # objectdiff::tracked_environment.
  if (!is.environment(context)) {
    stop("Please pass an ", sQuote("environment"), " as the context for ",
         "a stageRunner")
  }

  self$.parent <- NULL
  self$.finished <- FALSE # TODO: Remove this hack for printing
  self$.context <- context

  self$.mode <- tolower(mode)
  if (identical(remember, TRUE) && !(is.character(mode) &&
      any(self$.mode == c('head', 'next')))) {
    stop("The mode parameter to the stageRunner constructor must be ",
         "either 'head' or 'next'.")
  }

  if (length(.stages) == 0) {
    warning("stageRunners with zero stages may cause problems.", .call = FALSE)
  }

  if (!is_pre_stagerunner(.stages)) {
    stop("Can only turn a function or list of functions into a stageRunner", call. = FALSE)
  }

  if (is.function(.stages)) .stages <- list(.stages)
  self$stages <- .stages

  # Construct recursive stagerunners out of a list of lists.
  for (i in seq_along(self$stages))
    if (is.list(self$stages[[i]]))
      self$stages[[i]] <-
        stageRunner$new(self$.context, self$stages[[i]], remember = remember)
    else if (is.function(self$stages[[i]]) || is.null(self$stages[[i]]))
      self$stages[[i]] <- stageRunnerNode$new(self$stages[[i]], self$.context)

  # Do not allow the '/' character in stage names, as it's reserved for
  # referencing nested stages.
  if (any(violators <- grepl('/', names(self$stages), fixed = TRUE))) {
    msg <- paste0("Stage names may not have a '/' character. The following do not ",
      "satisfy this constraint: '",
      paste0(names(self$stages)[violators], collapse = "', '"), "'")
    stop(msg)
  }

  self$remember <- remember
  if (isTRUE(self$remember)) {
    # Set up parents for treeSkeleton.
    self$.clear_cache()
    self$.set_parents()
    if (self$with_tracked_environment()) {
      self$.set_prefixes()
    } else if (length(self$stages) > 0) {
      # Set the first cache environment
      first_env <- treeSkeleton$new(self$stages[[1]])$first_leaf()$object
      first_env$.cached_env <- new.env(parent = parent.env(self$.context))
      copy_env(first_env$.cached_env, self$.context)
    }
  }
}
