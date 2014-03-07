\name{nested_names}
\alias{nested_names}
\title{Delimited names of a nested list.}
\usage{
nested_names(el, delim = "/", prefix = "")
}
\arguments{
  \item{el}{list.}

  \item{delim}{character. The delimiter with which to
  separate nested names.}

  \item{prefix}{character. A prefix to every name.}
}
\value{
a list of nested names
}
\description{
Unnamed values will use index number instead.
}
\examples{
stagerunner:::nested_names(list(a = list(b = 1, c = list(d = 2, e = 3)), f = 4, 5))
# c('a/b', 'a/c/d', 'a/c/e', 'f', '3')
stagerunner:::nested_names(list(a = list(b = 1, c = 2), d = 2), delim = ' ', prefix = '#')
# c('#a b', '#a c', '#d')
}

