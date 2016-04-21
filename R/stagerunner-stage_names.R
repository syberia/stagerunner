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
  nested_stages <- function(x) {
    if (is.stagerunner(x)) {
      lapply(x$stages, nested_stages)
    } else {
      x
    }
  }

  nested_names(lapply(self$stages, nested_stages))
}

#' Delimited names of a nested list.
#'
#' Unnamed values will use index number instead.
#'
#' @name nested_names
#' @param el list.
#' @param delim character. The delimiter with which to separate nested names.
#' @param prefix character. A prefix to every name.
#' @return a list of nested names
#' @examples
#' stagerunner:::nested_names(list(a = list(b = 1, c = list(d = 2, e = 3)), f = 4, 5))
#' # c('a/b', 'a/c/d', 'a/c/e', 'f', '3')
#' stagerunner:::nested_names(list(a = list(b = 1, c = 2), d = 2), delim = ' ', prefix = '#')
#' # c('#a b', '#a c', '#d')
nested_names <- function(el, delim = '/', prefix = '') {
  list_names <- names(el) %||% rep("", length(el))
  Reduce(c, lapply(seq_along(el), function(index) {
    name <- if (list_names[[index]] == "") as.character(index)
            else list_names[[index]]
    paste0(prefix,
      if (is.list(el[[index]])) {
        paste0(name, delim, nested_names(el[[index]], delim = delim, prefix = ''))
      } else name)
  }))
}
