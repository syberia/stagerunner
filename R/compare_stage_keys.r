#' Compare two stage keys to see which one has a stage run first
#'
#' @param key1 list
#' @param key2 list
#' @return logical. Whether or not key1 runs a stage before key2.
compare_stage_keys <- function(key1, key2) {
  if (length(key1) == 0) return(TRUE)
  if (identical(key1, FALSE)) return(TRUE)
  if (identical(key2, FALSE)) return(FALSE)
  if (identical(key1, TRUE)) return(TRUE)
  if (identical(key2, TRUE))
    return(if (identical(key1[[1]], TRUE)) TRUE else compare_stage_keys(key1[[1]], key2))
  stopifnot(length(key1) == length(key2))
  if (length(key1) == 0) return(TRUE)
  ix <- vapply(list(key1, key2),
               function(el) which(vapply(el, contains_true, logical(1)))[1],
               numeric(1))
  stopifnot(is.finite(ix[1]) && is.finite(ix[2]))
  if (ix[1] == ix[2]) {
    if (is.atomic(key1) && is.atomic(key2)) return(TRUE) # tie!
    compare_stage_keys(key1[[ix[1]]], key2[[ix[2]]])
  }
  else ix[1] < ix[2]
}

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
  else ix[1] < ix[2]
}


