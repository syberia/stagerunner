set_first_cache <- function(stagerunner, cx, index = NULL) {
  prev_stage <- NULL
  stage <- stagerunner
  while (is.stagerunner(stage)) {
    prev_stage <- stage
    stage <- stage$stages[[1]]
  }
  if (is.null(prev_stage)) {
    parent.eval(substitute({
      .environment_cache[[index]] <<- new.env(parent = parent.env(context))
      copy_env(.environment_cache[[index]], context)
    }))
  } else {
    prev_stage$.environment_cache[[1]] <-
      new.env(parent = parent.env(prev_stage$context))
    copy_env(prev_stage$.environment_cache[[1]], cx)
  }
}

