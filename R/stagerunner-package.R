#' stagerunner: in-memory reproducible data preparation and modeling
#'
#' stagerunner is an attempt to define a notion of data munging that includes
#' \emph{history}. By writing your code as a stagerunner instead of a
#' collection of functions, three key advantages should become clear:
#'
#' \itemize{
#' \item Clarity will emerge in code that is intended to execute a sequence
#'   of operations that aims to produce a final result.
#' \item Reproducibility of interactive munging steps is possible without
#'   re-executing your analysis from scratch.
#' \item Modularity and extensibility becomes free of charge: methods like
#'   \code{around} and \code{transform} allow you to apply the same operation
#'   to your entire modeling procedure, simplifying progress monitoring and
#'   debugging.
#' }
#'
#' Although originally intended for clarifying the modeling process,
#' stagerunners have much more general applicability. To learn more,
#' begin with the vignettes: \code{browseVignettes(package = "stagerunner")}.
#'
#' @docType package
#' @name stagerunner
#' @import crayon R6
#' @author Robert Krzyzanowski <\url{http://syberia.io}>
#' @seealso The core function in this package: \code{\link{stagerunner}}. It
#'   defines the constructor creating stagerunner objects that allow you to
#'   wrap a complicated modeling procedure into an organized hierarchy.
#' @references Full documentation and demos: \url{http://robertzk.github.io/stagerunner/};
#'   FAQ's: \url{http://robertzk.github.io/stagerunner/faq/}
NULL

## Since `self` is used all over the place in R6 method definitions, 
## `R CMD CHECK` will yell at us if we do not include the line below.
globalVariables('self')
