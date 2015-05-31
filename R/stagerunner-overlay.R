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
