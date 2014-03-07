\name{boolean_fill}
\alias{boolean_fill}
\title{Fill a nested logical list with TRUEs before or after the first TRUE}
\usage{
boolean_fill(el, forward = TRUE)
}
\arguments{
  \item{el}{list. A nested list of logicals with exactly
  one entry TRUE.}

  \item{forward}{logical. FALSE for backwards, and TRUE for
  forwards.  The default is TRUE.}
}
\value{
the filled list
}
\description{
Fill a nested logical list with TRUEs before or after the
first TRUE
}

