\name{make.wide.format}
\alias{make.wide.format}
\title{Convert a 3-column data.frame into a matrix}
\usage{
make.wide.format(x)
}
\arguments{
  \item{x}{a data.frame, in which the first two columns (character) give the names of each pair of nodes, and the third (numeric or integer) gives the degree of association between those nodes.}
}
\value{
a matrix containing the value of the association function between each pair of nodes
}
\description{
Convert from 'long' format (3-column data.frame) to 'wide' format (matrix with names rows and cols). See make.long.format for the reverse.
}

