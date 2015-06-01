#' Whether or not the stageRunner has a key matching this input.
#'
#' @param key ANY. The potential key.
#' @return \code{TRUE} or \code{FALSE} accordingly.
stageRunner_has_key <- function(key) {
  ## We turn the key, like "data/foo" or `c(1,2)` into a
  ## nested list of logicals in the usual format, or `FALSE` if
  ## the key cannot be parsed.
  has <- tryCatch(normalize_stage_keys(key, self$stages),
                  error = function(.) FALSE)
  ## If any substage is `TRUE`, the stagerunner contains this key.
  ## Note that keys may refer to several different substages!
  ## This method will tell us whether it is possible to execute anything
  ## using the provided `key`.
  any(c(has, recursive = TRUE))
}

