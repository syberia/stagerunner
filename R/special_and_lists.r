#' AND two lists together with some regards for nesting
#'
#' The structure of the lists should be the same. That is,
#' as a tree, the two lists should be isomorphic. For example,
#' \code{special_and_lists(list(a = FALSE, b = list(b = TRUE, c = FALSE)),
#'                         list(a = FALSE, b = list(b = FALSE, c = TRUE)))}
#' yields
#' \code{list(a = FALSE, b = list(b = FALSE, c = TRUE))}
#' and
#' \code{special_and_lists(list(a = FALSE, b = list(b = TRUE, c = FALSE)),
#'                         list(a = list(b = FALSE, c = TRUE), b = FALSE))}
#' yields
#' \code{list(a = list(b = FALSE, c = TRUE), b = list(b = TRUE, c = FALSE))}
#'
#' Note that lists get ANDed based on *order*, not on key names (as this could
#' be ambiguous), so make sure the two lists have the same comparable key orders.
#' For example, \code{special_and_lists(list(a = TRUE, b = FALSE), list(b = FALSE, a = TRUE))}
#' would mistakenly return \code{list(a = TRUE, b = TRUE)}.
#'
#' @param list1 a list.
#' @param list2 a list.
#' @return the and'ed list.
special_and_lists <- function(list1, list2) {
  if (identical(list1, FALSE) || identical(list2, FALSE)) FALSE
  else if (identical(list1, TRUE)) list2
  else if (identical(list2, TRUE)) list1
  else if (!(is.list(list1) && is.list(list2)))
    stop("special_and_lists only accepts lists or atomic logicals of length 1")
  else if (length(list1) != length(list2)) {
    stop("special_and_lists only accepts lists of the same length")
  } else {
    if (!identical(names(list1), names(list2))) 
      warning("special_and_lists matches lists by order, not name, ",
              "but the names of the two lists do not match!")
    Map(special_and_lists, list1, list2)
  }
}


