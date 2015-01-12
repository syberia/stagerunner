`%||%` <- function(x, y) if (is.null(x)) y else x

contains_true <- function(x) {
  if (is.list(x)) any(vapply(x, contains_true, logical(1)))
  else any(x)
}

# Whether obj is of any of the given types.
is_any <- function(obj, klasses) {
  any(vapply(klasses, inherits, logical(1), x = obj))
}

package_function <- function(pkg, fn) { # for when using :: breaks R CMD check
  get(fn, envir = getNamespace(pkg))
}

#' Call a method on an object regardless of its OOP type.
#'
#' @name OOP_type_independent_method 
#' @param object any. An R object of variable OOP type (S3, S4, RC).
#' @param method character. The method to call on the \code{object}. If the
#'    latter is a reference class, it use the \code{$} operator to access the method.
#'    (For example, \code{object$some_method}). If it has an attribute with the name
#'    \code{method}, it will use that attribute as the method to call. Otherwise,
#'    it will try to fetch a generic with the name \code{method} using \code{get}.
OOP_type_independent_method <- function(object, method) {
  if (inherits(object, 'refClass')) {
    # bquote and other methods don't work here -- it's hard to dynamically
    # fetch reference class methods!
    eval(parse(text = paste0('`$`(object, "', method, '")()')))
  } else if (method %in% names(attributes(object)))
    attr(object, method)
  else get(method)(object)
}

# Stolen from testthat:::colourise
.fg_colours <- 
  structure(c("0;30", "0;34", "0;32", "0;36", "0;31", "0;35", "0;33",
  "0;37", "1;30", "1;34", "1;32", "1;36", "1;31", "1;35", "1;33",
  "1;37"), .Names = c("black", "blue", "green", "cyan", "red",
  "purple", "brown", "light gray", "dark gray", "light blue", "light green",
  "light cyan", "light red", "light purple", "yellow", "white"))
.bg_colours <- 
  structure(c("40", "41", "42", "43", "44", "45", "46", "47"), .Names = c("black",
  "red", "green", "brown", "blue", "purple", "cyan", "light gray"
  ))

colourise <- function (text, fg = "black", bg = NULL) {
  term <- Sys.getenv()["TERM"]
  colour_terms <- c("xterm-color", "xterm-256color", "screen",
      "screen-256color")
  if (!any(term %in% colour_terms, na.rm = TRUE)) return(text)
  col_escape <- function(col) paste0("\033[", col, "m")
  col <- .fg_colours[tolower(fg)]
  if (!is.null(bg)) col <- paste0(col, .bg_colours[tolower(bg)], sep = ";")
  init <- col_escape(col)
  reset <- col_escape("0")
  paste0(init, text, reset)
}

