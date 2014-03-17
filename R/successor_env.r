successor_env <- function(stagerunner, par = stagerunner$.parent) {
  if (is.stagerunner(stagerunner) &&
      !is.stagerunner(stagerunner$.parent)) return(FALSE)

  parent_index <- which(vapply(par$stages,
    function(stage) identical(stage, stagerunner), logical(1)))[1]
  stopifnot(is.finite(parent_index))

  if (parent_index == length(par$stages))
    successor_env(par)
  else {
    prev_stage <- NULL
    stage <- par$stages[[parent_index + 1]]
    while(is.stagerunner(stage)) {
      prev_stage <- stage
      stage <- head(stage$stages, 1)[[1]]
    }
    if (is.null(prev_stage)) {
      if (is.null(par$.environment_cache[[parent_index + 1]]))
        par$.environment_cache[[parent_index + 1]] <-
          new.env(parent = parent.env(par$context))

      par$.environment_cache[[parent_index + 1]]
    }
    else {
      if (is.null(head(prev_stage$.environment_cache, 1)[[1]]))
       prev_stage$.environment_cache[[1]] <- 
          new.env(parent = parent.env(prev_stage$context))

       prev_stage$.environment_cache[[1]]
    }
  }
}
