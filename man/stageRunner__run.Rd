\name{stageRunner__run}
\alias{stageRunner__run}
\title{Run the stages in a stageRunner object.}
\usage{
stageRunner__run(stage_key = NULL, to = NULL, normalized = FALSE,
  verbose = FALSE, remember_flag = TRUE)
}
\arguments{
  \item{stage_key}{an indexing parameter. Many forms are
  accepted, but the easiest is the name of the stage. For
  example, if we have \code{stageRunner$new(context,
  list(stage_one = some_fn, stage_two = some_other_fn))}
  then using \code{run('stage_one')} will execute
  \code{some_fn}.  Additional indexing forms are logical
  (which stages to execute), numeric (which stages to
  execute by indices), negative (all but the given stages),
  character (as above), and nested forms of these.  The
  latter refers to instances of the following:
  \code{stageRunner$new(context, list(stage_one =
  stageRunner$new(context, substage_one = some_fn,
  substage_two = other_fn), stage_two = another_fn))}.
  Here, the following all execute only substage_two:
  \code{run(list(list(FALSE, TRUE), FALSE))},
  \code{run(list(list(1, 2)))},
  \code{run('stage_one/substage_two')},
  \code{run('one/two')}, \code{run(list(list('one',
  'two')))}, \code{run(list(list('one', 2)))} Notice that
  regular expressions are allowed for characters.  The
  default is \code{NULL}, which runs the whole sequences of
  stages.}

  \item{to}{an indexing parameter. If \code{stage_key}
  refers to a single stage, attempt to run from that stage
  to this stage (or, if this one comes first, this stage to
  that stage). For example, if we have \code{stages =
  list(a = list(b = 1, c = 2), d = 3, e = list(f = 4, g =
  5))} where the numbers are some functions, and we call
  \code{run} with \code{stage_key = 'a/c'} and \code{to =
  'e/f'}, then we would execute stages \code{"a/c", "d",
  "e/f"}.}

  \item{normalized}{logical. A convenience recursion
  performance helper. If \code{TRUE}, stageRunner will
  assume the \code{stage_key} argument is a nested list of
  logicals.}

  \item{remember_flag}{logical. An internal argument used
  by \code{run} recursively if the \code{stageRunner}
  object has the \code{remember} field set to \code{TRUE}.
  If \code{remember_flag} is FALSE, \code{run} will not
  attempt to restore the context from cache (e.g., if we
  are executing five stages simultaneously with
  \code{remember = TRUE}, the first stage's context should
  be restored from cache but none of the remaining stages
  should).}

  \item{verbose}{logical. Whether or not to display pretty
  colored text informing about stage progress.}
}
\description{
Run the stages in a stageRunner object.
}

