\name{circleplot_fun}
\alias{circleplot}
\title{Plot a distance matrix}
\usage{
circleplot(distance.matrix, plot.control)
}
\arguments{
  \item{distance.matrix}{a distance matrix (class dist) containing either continuous or binary data}

  \item{plot.control}{a list giving information on how the plot should appear. If given, may contain any or all of the following named objects: 'points' = a data.frame giving point attributes, with colnames=c('label', "colour", "size"); 'line.gradient' = logical used to determine whether lines should display a colour gradient, which is only usually sensible for binary matrices; 'line.breaks' = vector used to determine the breaks for colours; 'line.colours' = vector containing colours. used with 'breaks' above, length= length(breaks)-1; 'line.curvature' = the percentage of maximum sent to gaussian curve i.e. curvature=1 always has a min=0, max=max, while curvature=0 is a straight line; 'line.width' = vector if length-1 contains absolute value; if length-2 contains minimum and max}
}
\value{
Draws a plot showing the labels of the distance matrix as nodes around the circumference of a circle, with quadratic lines joining connected nodes.
}
\description{
Draws a circular plot of a distance matrix.
}

