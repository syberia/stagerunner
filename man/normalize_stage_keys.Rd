\name{normalize_stage_keys}
\alias{normalize_stage_keys}
\title{Normalize a reference to stage keys}
\usage{
normalize_stage_keys(keys, stages, parent_key = "")
}
\arguments{
  \item{keys}{a list. The keys to normalize.}

  \item{stages}{a list. The stages we're normalizing with
  respect to.}

  \item{parent_key}{character. A helper for sane recursive
  error handling.  For example, if we try to reference key
  \code{foo/bar}, but a recursive call to
  \code{normalize_stage_keys} errors when \code{bar} isn't
  found, we would still like the error to display the full
  name (\code{foo/bar}).}
}
\value{
a list. The format is nested logicals. For example, if
\code{stages} is \code{list(one =
stageRunner$new(new.env(), list(subone = function(cx) 1)),
two = function(cx) 1)} then
\code{normalize_stage_keys('one/subone')} would return
\code{list(one = list(subone = TRUE), two = FALSE)}.
}
\description{
For example, \code{list('data/one', 2)} would be converted
to \code{list('data', list('one')), 2)}.
}
\examples{
\dontrun{
   # TODO: Fill in
}
}

