`%||%` <- function(x, y) if (is.null(x)) y else x

contains_true <- function(x) {
  if (is.list(x)) any(vapply(x, contains_true, logical(1)))
  else any(x)
}

# Whether obj is of any of the given types.
is_any <- function(obj, klasses) {
  any(vapply(klasses, inherits, logical(1), x = obj))
}

