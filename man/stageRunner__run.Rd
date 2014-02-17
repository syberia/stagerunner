\name{stageRunner__run}
\alias{stageRunner__run}
\title{Run the stages in a stageRunner object.}
\usage{
stageRunner__run(stage_key = NULL)
}
\arguments{
  \item{stage_key}{character. A string representing the
  stage to run (or re-run). This can be a regular
  expression. The default is \code{NULL}, which runs the
  whole sequences of stages. If the character is of length
  greater than 1, all matching stages will attempt to be
  run.}
}
\description{
Run the stages in a stageRunner object.
}

