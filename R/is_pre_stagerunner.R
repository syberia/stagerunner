is_pre_stagerunner <- function(x) {
  if (is.function(x) || is.stageRunner(x)) { return TRUE }
  if (!is.recursive(x)) { return FALSE }

  #is.function(x) || all(vapply(x,
  #  function(s) is.function(s) || is.stagerunner(s) || is.null(s) ||
   #   (is.list(s) && is_pre_stagerunner(s)), logical(1)))
}

