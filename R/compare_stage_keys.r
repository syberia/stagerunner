#' Compare two stage keys to see which one has a stage run first
#' 
#' This is a helper function to implement the \code{to} parameter
#' in the \code{run} method on a stageRunner object.
#'
#' @seealso \code{\link{stageRunner__run}}
#' @name compare_stage_keys
#' @param key1 list
#' @param key2 list
#' @return logical. Whether or not key1 runs a stage before key2.
compare_stage_keys <- function(key1, key2) {
  if (length(key1) == 0 || isTRUE(key1)) return(TRUE)
  ix <- vapply(list(key1, key2),
               function(el) which(vapply(el, contains_true, logical(1)))[1],
               numeric(1))
  if (!is.finite(ix[1])) return(FALSE)
  else if (!is.finite(ix[2])) return(TRUE)
  stopifnot(is.finite(ix[1]) && is.finite(ix[2]))
  if (ix[1] == ix[2]) {
    if (is.atomic(key1) && is.atomic(key2)) return(TRUE) # tie!
    compare_stage_keys(key1[[ix[1]]], key2[[ix[2]]])
  }
  else if (all(sapply(key2, isTRUE))) TRUE # This is a special case
  else ix[1] < ix[2]
}


