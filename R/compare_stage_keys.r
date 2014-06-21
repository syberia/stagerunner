#' Compare two stage keys to see which one has a stage run first
#' 
#' This is a helper function to implement the \code{to} parameter
#' in the \code{run} method on a stageRunner object.
#'
#' @name compare_stage_keys
#' @param key1 list
#' @param key2 list
#' @return logical. Whether or not key1 runs a stage before key2.
compare_stage_keys <- function(key1, key2) {
  if (length(key1) == 0) return(TRUE)
  if (identical(key1, TRUE)) return(TRUE)
  ix <- vapply(list(key1, key2),
               function(el) which(vapply(el, contains_true, logical(1)))[1],
               numeric(1))
  stopifnot(is.finite(ix[1]) && is.finite(ix[2]))
  if (ix[1] == ix[2]) {
    if (is.atomic(key1) && is.atomic(key2)) return(TRUE) # tie!
    compare_stage_keys(key1[[ix[1]]], key2[[ix[2]]])
  }
  else if (all(sapply(key2, identical, TRUE))) TRUE # This is a special case
  else ix[1] < ix[2]
}


