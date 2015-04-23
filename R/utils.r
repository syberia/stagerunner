`%||%` <- function(x, y) if (is.null(x)) y else x

contains_true <- function(x) {
  if (is.list(x)) any(vapply(x, contains_true, logical(1)))
  else any(x)
}

enforce_type <- function(value, expected, klass, name = deparse(substitute(value))) {
  if (missing(value)) {
    stop(sprintf(
      "Please provide %s%s.",
      articleize(sQuote(crayon::red(name))),
      if (missing(klass)) "" else paste( " to a", klass)
    ))
  }

  check <- utils::getFromNamespace(paste0("is.", expected), "base")
  if (!check(value)) {
    stop(sprintf(
      "Please pass %s as the %s%s; instead I got a %s.",
      articleize(sQuote(crayon::yellow(expected))), dQuote(name),
      if (missing(klass)) "" else paste(" for a", klass),
      crayon::red(sclass(value))
    ))
  }
}

sclass <- function(obj) { class(obj)[1L] }

articleize <- function(word) {
  sprintf("a%s %s", if (is_vowel(first_letter(word))) "n" else "", word)
}

is_vowel <- function(char) {
  is.element(char, c("a", "e", "i", "o", "u", "A", "E", "I", "O", "U"))
}

first_letter <- function(word) {
  substring(gsub("[^a-zA-Z]|\\[3[0-9]m", "", word), 1, 1)
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
  if (method %in% names(attributes(object))) {
    attr(object, method)
  } else if (is.environment(object) && method %in% ls(object)) {
    object[[method]]()
  } else {
    get(method)(object)
  }
}

# Convert an environment to a list
as.list.environment <- function(env) {
  out <- base::as.list.environment(env)
  lapply(out, function(x) if (is.environment(x)) as.list(x) else x)
}

