#' Delimited names of a nested list.
#'
#' Unnamed values will use index number instead.
#'
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
      if (is.list(el[[index]]))
        paste0(name, delim, nested_names(el[[index]], delim = delim, prefix = ''))
      else name)
  }))
}
