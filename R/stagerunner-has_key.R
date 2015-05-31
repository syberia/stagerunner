#' Whether or not the stageRunner has a key matching this input.
#'
#' @param key ANY. The potential key.
#' @return \code{TRUE} or \code{FALSE} accordingly.
stageRunner_has_key <- function(key) {
  has <- tryCatch(normalize_stage_keys(key, self$stages), error = function(.) FALSE)
  any(c(has, recursive = TRUE))
}

