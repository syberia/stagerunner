`%||%` <- function(x, y) if (is.null(x)) y else x

contains_true <- function(x) {
  if (is.list(x)) any(vapply(x, contains_true, logical(1)))
  else any(x)
}

# Whether obj is of any of the given types.
is_any <- function(obj, klasses) {
  any(vapply(klasses, inherits, logical(1), x = obj))
}

#' Call a method on an object regardless of its OOP type.
OOP_type_independent_method <- function(object, method) {
  if (inherits(object, 'refClass')) {
    # bquote and other methods don't work here -- it's hard to dynamically
    # fetch reference class methods!
    eval(parse(text = paste0('`$`(object, "', method, '")()')))
  } else if (method %in% names(attributes(object)))
    attr(object, method)
  else get(method)(object)
}

