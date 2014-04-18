\name{show_message}
\alias{show_message}
\title{Show a progress message.}
\usage{
show_message(stage_names, stage_index, begin = TRUE)
}
\arguments{
  \item{stage_names}{character.}

  \item{stage_index}{integer.}

  \item{begin}{logical. Whether we are showing the begin or
  end message.}
}
\value{
the message to standard output
}
\description{
Show a progress message.
}
\examples{
\dontrun{
show_message(c('one', 'two'), 2) # Will print "Beginning one stage..."
}
}

