\docType{class}
\name{stageRunnerNode-class}
\alias{stageRunnerNode-class}
\title{Stagerunner nodes are environment wrappers around individual stages
(i.e. functions) in order to track meta-data (e.g., for caching).}
\arguments{
  \item{fn}{function. This will be wrapped in an
  environment.}

  \item{parent_obj}{stageRunner. The enclosing stageRunner
  object.}

  \item{parent_env}{environment. The parent environment of
  the created \code{stageRunnerNode} object. The default is
  the calling environment (i.e., \code{parent.frame()}).}
}
\value{
an environment with some additional attributes for
navigating in a tree-like structure.
}
\description{
Stagerunner nodes are environment wrappers around
individual stages (i.e. functions) in order to track
meta-data (e.g., for caching).
}

