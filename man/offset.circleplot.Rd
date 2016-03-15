\name{offset.circleplot}
\alias{offset.circleplot}
\title{Change the origin and scale of an object returned by circleplot}
\usage{
offset.circleplot(x, offset.x, offset.y, scale)
}
\arguments{
  \item{x}{an object returned by circleplot}
  \item{offset.x}{new location of the origin on the x axis}
  \item{offset.y}{new location of the origin on the y axis}
  \item{scale}{how much larger should the new plot be?}
}
\value{
Returns a list with the same components as that returned from circleplot, but relocated as specified by the user
}
\description{
Move a circleplot - useful for adding many plots to the same region
}

