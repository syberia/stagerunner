\name{stageRunner__initialize}
\alias{stageRunner__initialize}
\title{Initialize a stageRunner object.}
\usage{
stageRunner__initialize(context, .stages, remember = FALSE)
}
\arguments{
  \item{context}{an environment. The initial environment
  that is getting modified during the execution of the
  stages.}

  \item{.stages}{a list. The functions to execute on the
  \code{context}.}

  \item{remember}{a logical. Whether to keep a copy of the
  context and its contents throughout each stage for
  debugging purposes--this makes it easy to go back and
  investigate a stage. This could be optimized by
  developing a package for "diffing" two environments. The
  default is \code{FALSE}. When set to \code{TRUE}, the
  return value of the \code{run} method will be a list of
  two environments: one of what the context looked like
  before the \code{run} call, and another of the
  aftermath.}
}
\description{
stageRunner objects are used for executing a linear
sequence of actions on a context (an environment). For
example, if we have an environment \code{e} containing
\code{x = 1, y = 2}, then using \code{stages =
list(function(e) e$x <- e$x + 1, function(e) e$y <- e$y -
e$x)} will cause \code{x = 2, y = 0} after running the
stages.
}

