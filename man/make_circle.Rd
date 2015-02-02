\name{make.circle}
\alias{make_circle}
\title{Generate coordinates of points on the circumference of a circle}
\usage{
make.circle(n, alpha, k)
}
\arguments{
  \item{n}{The number of points to place on the circumference}

  \item{alpha}{Angle (in degrees) by which to rotate point coordinates. Defaults to 22.91 (0.4 radians)}

  \item{k}{The radius; defaults to one}
}
\value{
Returns a data.frame with x & y coordinates, as well as the angle (in radians) of that point from the origin.
}
\description{
This function is used to generate point locations in circleplot; but can also be useful for adding furter points and labels to an existing plot.
}