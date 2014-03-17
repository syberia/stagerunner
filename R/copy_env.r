copy_env <- function(to, from) {
  stopifnot(is.environment(to) && is.environment(from))
  rm(list = ls(to, all.names = TRUE), envir = to)
  for (name in ls(from, all.names = TRUE)) assign(name, from[[name]], envir = to)
}

