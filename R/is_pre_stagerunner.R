#' Whether or not an object can be transformed into a stageRunner.
#'
#' @param x ANY. An R object for which it will be determined whether or not
#'    it admits a conversion to a stageRunner.
#' @return TRUE or FALSE according to whether the object can be transformed
#'    to a stageRunner. In general, only a function or list of functions
#'    can be turned into a stageRunner.
#' @export
#' @examples
#' stopifnot(is_pre_stagerunner(function(e) { e$x <- 1 }))
#' stopifnot(is_pre_stagerunner(list(function(e) { e$x <- 1 }, function(e) { e$y <- 2 })))
#' stopifnot(is_pre_stagerunner(
#'   list(a = function(e) { e$x <- 1 },
#'     list(b = function(e) { e$y <- 2 }, c = function(e) { e$z <- 3 }))))
#' 
#' stopifnot(!is_pre_stagerunner(NULL))
#' stopifnot(!is_pre_stagerunner(5))
#' stopifnot(!is_pre_stagerunner(iris))
is_pre_stagerunner <- function(x) {
  if (is.function(x) || is.stagerunner(x)) { return(TRUE) }
  if (!is.recursive(x)) { return(FALSE) }

  for (i in seq_along(x)) {
    if (!(is.function(x[[i]]) || is.stagerunner(x[[i]]) || is.null(x[[i]]) ||
          (is.recursive(x[[i]]) && Recall(x[[i]])))) {
      return(FALSE)
    }
  }

  TRUE
}

