#' stagerunner: in-memory reproducible data preparation and modeling
#'
#' stagerunner is an attempt to define a notion of data munging that includes
#' \emph{history}. By writing your code as a stagerunner instead of a
#' collecton of functions, three key advantages should become clear:
#'
#' \itemize{
#' \item Clarity will emerge in code that is intended to execute a sequence
#'   of operations to obtain a final result.
#' \item Reproducibility of interactive munging steps is possible without
#'   re-executing your analysis from scratch.
#' \item Modularity and extensibility becomes free of charge: methods like
#'   \code{around} and \code{transform} allow you to apply the same operation
#'   to your entire modeling procedure.
#' }
#'
#' Although originally intended for clarifying the modeling process,
#' stagerunners have much more general applicability. To learn more,
#' begin with the vignettes: \code{browseVignettes(package = "stagerunner")}.
#'
#' @name stagerunner
#' @import testthat testthatsomemore crayon R6
#' @docType package
NULL
