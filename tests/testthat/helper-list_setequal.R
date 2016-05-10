#' Test that two lists are the same as named sets.
#'
#' @param list1 list.
#' @param list2 list.
#' @return TRUE if the lists are the same as named sets,
#'    and FALSE otherwise.
list_setequal <- function(list1, list2) {
  if (is.null(names(list1)) || is.null(names(list2))) {
    stop("Only named lists can be compared using list_setequal.")
  }
  stopifnot(all(nzchar(list1)), all(nzchar(list2)))
  stopifnot(setequal(names(list1), names(list2)))
  isTRUE(all.equal(list1, list2[names(list1)] ))
}

