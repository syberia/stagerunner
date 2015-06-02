## Overlaying a stagerunner means replacing the terminal nodes with
## terminal nodes that do some extra behavior, and can themselves
## be full stagerunners. This is subtly different than the `around`
## method, which transforms the terminal node function itself, rather
## than turning it from a function to a stagerunner.
## 
## For example, if we have a stagerunner like
##
##   * import data
##   * clean data
##      * impute variable 1
##      * discretize variable 2
##   * train model
## 
## we may wish to replace each function with a "hidden" mini-runner
## that runs some tests after each stage.
##
##   * import data
##     * vanilla function
##     * some testing function that checks data got imported
##   * clean data
##     * impute variable 1
##        * vanilla function
##        * some testing function that checks variables were imputed
##     * discretize variable 2
##        * vanilla function
##        * some testing function that checks variables were discretized
##   * train model
##     * vanilla function
##     * some testing function that checks the model got trained successfully
## 
## We can achieve this by passing the stagerunner with the same tree structure
## but containing tests in the terminal node as the argument to the main
## stagerunner's `overlay` method.
#' Overlaying a stageRunner object is taking another stageRunner object
#' with similar stage names and adding the latter's stages as terminal stages
#' to the former (for example, to support tests).
#'
#' @name stageRunner_overlay
#' @param other_runner stageRunner. Another stageRunner from which to overlay.
#' @param label character. The label for the overlayed stageRunner. This refers
#'    to the name the former will get wrapped with when appended to the
#'    stages of the current stageRunner. For example, if \code{label = 'test'},
#'    and a current terminal node is unnamed, it will becomes
#'    \code{list(current_node, test = other_runner_node)}.
#' @param flat logical. Whether to use the \code{stageRunner$append} method to
#'    overlay, or simply overwrite the given \code{label}. If \code{flat = TRUE},
#'    you must supply a \code{label}. The default is \code{flat = FALSE}.
stageRunner_overlay <- function(other_runner, label = NULL, flat = FALSE) {
  stopifnot(is.stagerunner(other_runner))
  for (stage_index in seq_along(other_runner$stages)) {
    name <- names(other_runner$stages)[[stage_index]]
    index <-
      if (identical(name, '') || identical(name, NULL)) stage_index
      else if (name %in% names(self$stages)) name
      else stop('Cannot overlay because keys do not match')
    self$stages[[index]]$overlay(other_runner$stages[[stage_index]], label, flat)
  }
  TRUE
}

