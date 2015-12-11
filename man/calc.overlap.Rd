\name{calc.overlap}
\alias{calc.overlap}
\title{Determine which nodes are shared between entries of a list}
\usage{
calc.overlap(x)
}
\arguments{
  \item{x}{a list containing >1 matrices or data.frames, each containing information on pairwise associations (see circleplot)}
}
\value{
Returns a data.frame listing all nodes mentioned in any of the entries of x, with one column per entry stating whether that node is present
}
\description{
Compare nodes present in different entries of a list.
}