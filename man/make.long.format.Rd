\name{make.long.format}
\alias{make.long.format}
\title{Convert a matrix of pairwise associations into a 3-column data.frame}
\usage{
make.long.format(x)
}
\arguments{
  \item{x}{a matrix containing the value of the association function between each pair of nodes}
}
\value{
Returns a data.frame, in which the first two columns (character) give the names of each pair of nodes, and the third (numeric or integer) gives the degree of association between those nodes.
}
\description{
Convert from 'wide' format (matrix with names rows and cols) to 'long' format (3-column data.frame). See make.wide.format for the reverse.
}

