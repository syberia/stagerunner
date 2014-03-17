copy_env <- function(to, from) {
  to <- as.environment(as.list(from, all.names = TRUE))
}
