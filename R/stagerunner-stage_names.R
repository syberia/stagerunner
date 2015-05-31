#' Retrieve a flattened list of canonical stage names for a stageRunner object
#'
#' For example, if we have stages
#'   \code{stages = list(a = list(b = 1, c = 2), d = 3, e = list(f = 4, g = 5))}
#' then this method would return
#'   \code{list('a/b', 'a/c', 'd', 'e/f', 'e/g')}
#'
#' @name stageRunner_stage_names
#' @return a list of canonical stage names.
# # @examples
# # f <- function() {}
# # sr <- stageRunner$new(new.env(),
# #   list(a = stageRunner$new(new.env(), list(b = f, c = f)), d = f,
# #   e = stageRunner$new(new.env(), list(f = f, g = f))))
# # sr$stage_names()
stageRunner_stage_names <- function() {
  nested_stages <- function(x) if (is.stagerunner(x)) nested_stages(x$stages) else x
  nested_names(lapply(self$stages, nested_stages))
}


