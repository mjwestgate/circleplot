\name{point_attr}
\alias{point.attr}
\title{List of point colours}
\usage{
point.attr(distance.matrix)
}
\arguments{
  \item{distance.matrix}{a distance matrix (class dist) containing either continuous or binary data}
}
\value{
Returns a data.frame of the approriate format for the plot.control$points command in circleplot().
}
\description{
Selects some pretty colours for point vertices from RColorBrewer. Not a sophisticated function by any means.
}

