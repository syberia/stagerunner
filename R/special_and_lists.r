## Imagine we want to run from "create validation set" to "create derived
## variable."
##
##  * import data
##  * __create validation set__
##  * __munge data__
##    * __impute variable 1__
##    * __create derived variable__
##    * drop some variables
##  * train model
## 
## The syntax for this is `runner$run("val", "munge/derived")` (amongst
## other ways -- substrings are matched to stage names by the 
## `normalize_stage_keys` helper).
##
## To translate this into code, stagerunner builds the following two trees:
##
##  * `FALSE`
##  * __`TRUE`__
##    * __`TRUE`__
##    * __`TRUE`__
##    * __`TRUE`__
##  * __`TRUE`__
##
## and
## 
##  * __`TRUE`__
##  * __`TRUE`__
##    * __`TRUE`__
##    * __`TRUE`__
##    * `FALSE`
##  * `FALSE`
##
## and then *intersects them*:
##
##  * `FALSE`
##  * __`TRUE`__
##    * __`TRUE`__
##    * __`TRUE`__
##    * `FALSE`
##  * `FALSE`
## 
## The point of `special_and_lists` is to perform this intersection operation.
##
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
#' @name special_and_lists
#' @param list1 a list.
#' @param list2 a list.
#' @seealso \code{\link{special_or_lists}}
#' @return the and'ed list.
#' @examples \dontrun{
#'   stopifnot(identical(
#'     special_and_lists(list(a = FALSE, b = list(b = TRUE, c = FALSE)),
#'                       list(a = FALSE, b = list(b = FALSE, c = TRUE))),
#'     list(a = FALSE, b = list(b = FALSE, c = TRUE))
#'  ))
#' 
#'   stopifnot(identical(
#'     special_and_lists(list(a = FALSE, b = list(b = TRUE, c = FALSE)),
#'                       list(a = list(b = FALSE, c = TRUE), b = FALSE)),
#'     list(a = list(b = FALSE, c = TRUE), b = list(b = TRUE, c = FALSE))
#'  ))
#' }
special_and_lists <- function(list1, list2) {
  if (identical(list1, FALSE) || identical(list2, FALSE)) {
    FALSE
  ## If one of the two lists is `TRUE`, an "AND" operation is simply
  ## equivalent to choosing the other list.
  } else if (identical(list1, TRUE)) {
    list2
  } else if (identical(list2, TRUE)) {
    list1
  } else if (!(is.list(list1) && is.list(list2))) {
    stop("special_and_lists only accepts lists or atomic logicals of length 1")
  } else if (length(list1) != length(list2)) {
    stop("special_and_lists only accepts lists of the same length")
  } else {
    ## This function should only ever be used on lists coming from the same
    ## hierarchy of stages, so give a warning if this is not the case.
    if (!identical(names(list1), names(list2))) {
      warning("special_and_lists matches lists by order, not name, ",
              "but the names of the two lists do not match!")
    }
    Map(special_and_lists, list1, list2)
  }
}


