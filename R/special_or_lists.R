## This function is equivalent to `special_and_lists` but instead we apply 
## "OR" to each pair of logical values:
## 
##  * `FALSE`
##  * __`TRUE`__
##    * __`TRUE`__
##    * __`TRUE`__
##    * `FALSE`
##  * `FALSE`
##
## and
## 
##  * `FALSE`
##  * `FALSE`
##    * `FALSE`
##    * __`TRUE`__
##    * __`TRUE`__
##  * `FALSE`
##
## would become
##
##  * `FALSE`
##  * __`TRUE`__
##    * __`TRUE`__
##    * __`TRUE`__
##    * __`TRUE`__
##  * `FALSE`
## 
#' OR two lists together with some regards for nesting
#'
#' The structure of the lists should be the same. That is,
#' as a tree, the two lists should be isomorphic. For example,
#' \code{special_or_lists(list(a = FALSE, b = list(b = TRUE, c = FALSE)),
#'                         list(a = FALSE, b = list(b = FALSE, c = TRUE)))}
#' yields
#' \code{list(a = FALSE, b = list(b = TRUE, c = TRUE))}
#' and
#' \code{special_or_lists(list(a = FALSE, b = list(b = TRUE, c = FALSE)),
#'                         list(a = list(b = FALSE, c = TRUE), b = FALSE))}
#' yields
#' \code{list(a = list(b = FALSE, c = TRUE), b = list(b = TRUE, c = FALSE))}
#'
#' Note that lists get ORed based on *order*, not on key names (as this could
#' be ambiguous), so make sure the two lists have the same comparable key orders.
#' For example, \code{special_or_lists(list(a = TRUE, b = FALSE), list(b = FALSE, a = TRUE))}
#' would mistakenly return \code{list(a = TRUE, b = TRUE)}.
#'
#' @name special_or_lists
#' @param list1 a list.
#' @param list2 a list.
#' @seealso \code{\link{special_and_lists}}
#' @return the or'ed list.
special_or_lists <- function(list1, list2) {
  if (identical(list1, TRUE) || identical(list2, TRUE)) {
    TRUE
  } else if (identical(list1, FALSE)) {
    list2
  } else if (identical(list2, FALSE)) {
    list1
  } else if (!(is.list(list1) && is.list(list2))) {
    stop("special_or_lists only accepts lists or atomic logicals of length 1")
  } else if (length(list1) != length(list2)) {
    stop("special_or_lists only accepts lists of the same length")
  } else {
    if (!identical(names(list1), names(list2))) {
      warning("special_or_lists matches lists by order, not name, ",
              "but the names of the two lists do not match!")
    }
     
    Map(special_or_lists, list1, list2)
  }
}


