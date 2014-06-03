#' Copy one environment into another recursively.
#' 
#' @name copy_env
#' @param to environment. The new environment.
#' @param from environment. The old environment.
copy_env <- function(to, from) {
  stopifnot(is.environment(to) && is.environment(from))
  rm(list = ls(to, all.names = TRUE), envir = to)
  for (name in ls(from, all.names = TRUE)) {
    if (is.environment(from[[name]])) {
      # Copy a sub-environment in full.
      assign(name, new.env(parent = parent.env(from[[name]])), envir = to) 
      copy_env(to[[name]], from[[name]])
    } else assign(name, from[[name]], envir = to)
  }
}

