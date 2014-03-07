#' Fill a nested logical list with TRUEs before or after the first TRUE
#'
#' @param el list. A nested list of logicals with exactly one entry TRUE.
#' @param direction logical. FALSE for backwards, and TRUE for forwards.
#'   The default is TRUE.
#' @return the filled list
boolean_fill <- function(el, direction = TRUE) {
  contains_true <-
    function(x) if (is.list(x)) any(vapply(x, contains_true, logical(1)))
                else identical(x, TRUE)
  ix <- which(vapply(el, contains_true, logical(1)))[1]
  if (!is.finite(ix)) stop("boolean_fill called but no TRUEs found")
  fills <-
    if (direction) seq_len(length(el) - ix) + ix
    else seq_len(ix - 1)
  el[fills] <- TRUE
  if (!is.atomic(el)) el[[ix]] <- boolean_fill(el[[ix]], direction = direction)
  el
}




