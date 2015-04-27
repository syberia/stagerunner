## To determine what stages of a stagerunner to execute, we will use a nested
## list format that is equivalent in structure to the runner. For example,
## imagine we have a stagerunner with the following stages:
##
##   * import data
##   * clean data
##      * impute variable 1
##      * discretize variable 2
##   * train model
## 
## We would like to be able to execute swaths of this runner at will:
## `runner$run("clean")`, `runner$run("clean/1", "clean/2")` and 
## `runner$run(2)` should all execute the data cleaning sub-stages.
##
## The `normalize_stage_keys` function will convert human-readable
## descriptions of what to execute, like `"clean"` or `2`, to a
## nested list format that will be easier to use later during stage
## execution.
##
## For example, `"clean/1"` will be converted to
## `list(F, list(T, F), F)` and mimic the structure of the stagerunner.
#' Normalize a reference to stage keys 
#'
#' For example, \code{list('data/one', 2)} would be converted to
#' \code{list('data', list('one')), 2)}.
#'
#' @name normalize_stage_keys
#' @param keys a list. The keys to normalize.
#' @param stages a list. The stages we're normalizing with respect to.
#' @param parent_key character. A helper for sane recursive error handling.
#'    For example, if we try to reference key \code{foo/bar}, but a recursive
#'    call to \code{normalize_stage_keys} errors when \code{bar} isn't found,
#'    we would still like the error to display the full name (\code{foo/bar}).
#' @param to an indexing parameter. If \code{keys} refers to a single stage,
#'   attempt to find all stages from that stage to this stage (or, if this one
#'   comes first, this stage to that stage). For example, if we have
#'      \code{stages = list(a = list(b = 1, c = 2), d = 3, e = list(f = 4, g = 5))}
#'   where the numbers are some functions, and we call \code{normalize_stage_keys}
#'   with \code{keys = 'a/c'} and \code{to = 'e/f'}, then we would obtain a nested
#'   list of logicals referencing \code{"a/c", "d", "e/f"}.
#' @return a list. The format is nested logicals. For example, if \code{stages} is
#'   \code{list(one = stageRunner$new(new.env(), list(subone = function(cx) 1)),
#'              two = function(cx) 1)}
#' then
#'   \code{normalize_stage_keys('one/subone')}
#' would return
#'   \code{list(one = list(subone = TRUE), two = FALSE)}.
#' @seealso stageRunner__run
#' @examples
#' \dontrun{
#'   stopifnot(identical(normalize_stage_keys("foo/bar",
#'     list(foo = list(bar = NULL, baz = NULL))),
#'     list(list(TRUE, FALSE))))
#' }
normalize_stage_keys <- function(keys, stages, to = NULL, parent_key = "") {
  if (is.null(to)) {
    normalize_stage_keys_unidirectional(keys, stages, parent_key)
  } else {
    normalize_stage_keys_bidirectional(keys, to, stages)
  }
}

normalize_stage_keys_unidirectional <- function(keys, stages, parent_key) {
  if (is.null(keys) || length(keys) == 0 || identical(keys, "")) {
    ## By default, no key provided means to execute everything in this stage.
    ## For single stages, that means `TRUE`. For multiple stages, a
    ## list of `TRUE`s equal to the number of stages.
    return(if (!is.list(stages) || length(stages) == 1) TRUE
           else rep(list(TRUE), length(stages)))
  }
  ## Stagerunners are set up recursively, so we need to extract the `list` of
  ## stages out of the stagerunner object. For example, 
  ##
  ##   * import data
  ##   * clean data
  ##      * impute variable 1
  ##      * discretize variable 2
  ##
  ## actually consists of *two* stagerunners, one for the whole list and one
  ## for the "clean data" stage.
  if (is.stagerunner(stages)) stages <- stages$stages

  ## The output of `normalize_stage_keys` is a (possibly nested) list whose
  ## terminal nodes are all logical. If `keys` is already of this format, we
  ## are done.
  if (all_logical(keys)) return(keys) # Already normalized

  ## We performed checks for special cases, so now we call a function that
  ## assumes all those cases have been taken care of. 
  normalize_stage_keys_unidirectional_(keys, stages, parent_key)
}

## Our strategy to determine which stages to run, and thus translate `keys`
## from a form like "munge/impute variable 5" to a nested list, will be to
## start with a list of consisting entirely of `FALSE` and filling in the
## sub-stages the user requested in the `keys` with `TRUE`s.
normalize_stage_keys_unidirectional_ <- function(keys, stages, parent_key) {
  ## Negative indexing, like `-c(2:3)`, is easy: set everything *except* 
  ## those keys to `TRUE`.
  if (is.numeric(keys) && any(keys < 0)) { 
    as.list(!is.element(seq_len(stage_length(stages)), -keys))
  } else {
    key_length      <- if (is.list(stages)) length(stages) else 1
    normalized_keys <- rep(list(FALSE), key_length)

    ## Each element of the provided keys has a chance to modify
    ## `normalized_keys`. We achieve this using [Reduce](https://stat.ethz.ch/R-manual/R-devel/library/base/html/funprog.html).
    Reduce(function(normalized_keys, index) {
      normalize_stage_key(keys, stages, parent_key, index, normalized_keys)
    }, seq_along(keys), normalized_keys)
  }
}

normalize_stage_key <- function(keys, stages, parent_key, key_index, normalized_keys) {
  seqs <- seq_along(if (is.list(stages)) stages else 1)
  key  <- keys[[key_index]]
  if (length(key) == 0) stop("Invalid stage key (cannot be of length 0)")
  rest_keys <- key[-1]
  key <- key[[1]]
  if (is.logical(key)) {
    normalized_keys[[key_index]] <- key
  } else if (is.numeric(key) && key %in% seqs) {
    # Recursively process any potential extra keys
    normalized_keys[[as.integer(key)]] <-
      if (length(rest_keys) == 0) TRUE
      else normalize_stage_keys(rest_keys, stages[[as.integer(key)]],
                                parent_key = paste0(parent_key, key, '/'))
  } else if (is.character(key)) {
    # The hard part! Allow things like one/subone/subsubone/etc
    # to reference arbitrarily nested stages.
    if (length(key) == 0) stop("Stage key of length zero")
    key <- strsplit(key, '/')[[1]]

    if (is.stageRunnerNode(stages)) {
      stop("No stage with key '", paste0(parent_key, key[[1]]), "' found")
    }

    key_index <- grepl(key[[1]], names(stages), ignore.case = TRUE, fixed = TRUE)
    if (is.finite(suppressWarnings(tmp <- as.numeric(key[[1]]))) &&
        tmp > 0 && tmp <= length(stages)) key_index <- tmp
    else if (length(key_index) == 0 || sum(key_index) == 0) {
      stop("No stage with key '", paste0(parent_key, key[[1]]), "' found")
    } else if (sum(key_index) > 1) {
      stop("Multiple stages with key '", paste0(parent_key, key[[1]]),
             "', found: ", paste0(parent_key, names(stages)[key_index], collapse = ', '))
    } else key_index <- which(key_index) # now an integer of length 1

    normalized_keys[[key_index]] <- special_or_lists(
      normalized_keys[[key_index]],
      normalize_stage_keys(append(paste0(key[-1], collapse = '/'), rest_keys), 
        stages[[key_index]], parent_key = paste0(parent_key, key[[1]], '/')))
  } else stop("Invalid stage key")

  normalized_keys
}

normalize_stage_keys_bidirectional <- function(from, to, stages) {
  ## First, we turn our human-readable keys like "clean/impute variable 1"
  ## into a more convenient list structure like `list(F, list(T, F, F), F)`.
  from <- normalize_stage_keys(from, stages)
  to   <- normalize_stage_keys(to, stages)
  ## Recall our helper `compare_stage_keys`, which returns `FALSE` if 
  ## the first argument occurs before the second. In this situation, we
  ## need to swap the keys.
  if (!compare_stage_keys(from, to)) {
    ## A convenient swapping mechanism without introducing temporary variables.
    ## In R, the [`list2env`](https://stat.ethz.ch/R-manual/R-devel/library/base/html/list2env.html) utility
    ## can funnel named values in a list directly into an environment.
    ## Try it yourself:
    ## ```r
    ## x <- 1
    ## y <- 2
    ## list2env(list(x = y, y = x), environment())
    ## cat(x, ",", y)
    ## ```
    list2env(list(from = to, to = from), environment())
  }
  ## And finally the magic trick that pulls it all together. See the more
  ## thorough explanation below beside the `special_and_lists` helper.
  special_and_lists(
    boolean_fill(from, forward = TRUE),
    boolean_fill(to,   forward = FALSE)
  )
}

## Terminal stages in a stagerunner are `stageRunnerNode` objects, so we treat
## those as stages of length 1.
stage_length <- function(obj) {
  if (is.list(obj)) length(obj)
  else 1
}

