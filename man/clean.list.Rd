\name{clean.list}
\alias{clean.list}
\title{Convert a list of association matrices or data.frames to ensure the resulting circleplots have identical nodes}
\usage{
clean.list(x, type)
}
\arguments{
  \item{x}{a list containing >1 matrices or data.frames, each containing information on pairwise associations (see circleplot)}

  \item{type}{should the function return entries containing all nodes present in any of the list enties (type="OR", the default), or only those nodes that are present in all entries (type="AND")?}
}
\value{
Returns a list with the same length as x, but where each entry is a matrix with the same dimensions and dimnames
}
\description{
Function to increase consistency and comparability of related datasets when drawn using circleplot.
}