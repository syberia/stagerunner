\name{stageRunner__stage_names}
\alias{stageRunner__stage_names}
\title{Retrieve a flattened list of canonical stage names for a stageRunner object}
\usage{
stageRunner__stage_names()
}
\value{
a list of canonical stage names.
}
\description{
For example, if we have stages \code{stages = list(a =
list(b = 1, c = 2), d = 3, e = list(f = 4, g = 5))} then
this method would return \code{list('a/b', 'a/c', 'd',
'e/f', 'e/g')}
}
\examples{
f <- function() {}
sr <- stageRunner$new(new.env(),
  list(a = stageRunner$new(new.env(), list(b = f, c = f)), d = f,
  e = stageRunner$new(new.env(), list(f = f, g = f))))
sr$stage_names()
}

