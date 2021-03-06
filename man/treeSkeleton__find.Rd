% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/treeSkeleton.R
\name{treeSkeleton__find}
\alias{treeSkeleton__find}
\title{Find the key with the given index using the names of the lists
that parametrize each node's children.}
\usage{
treeSkeleton__find(key)
}
\arguments{
\item{key}{character. The key to find in the given tree structure,
whether nodes are named by their name in the \code{children()}
list. Numeric indices can be used to refer to unnamed nodes.
For example, if key is \code{a/2/b}, this method would try to find
the current node's child \code{a}'s second child's \code{b} child.
(Just look at the examples).}
}
\value{
the subtree or terminal node with the given key.
}
\description{
For example, if our tree structure is given by
  \code{list(a = list(b = 1, c = 2))}
then calling \code{find('a/b')} on the root node will return \code{1}.
}
\examples{
\dontrun{
sr <- stageRunner$new(new.env(), list(a = list(force, list(b = function(x) x + 1))))
stagerunner:::treeSkeleton$new(sr)$find('a/2/b') # function(x) x + 1
}
}
