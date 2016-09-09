\name{draw_circle}
\alias{draw.circle}
\title{draw a circle with specified attributes}
\usage{
draw.circle(k=1, x0=0, y0=0, filled=FALSE, n=100, trim=0, alpha=0, ...)
}
\arguments{
  \item{k}{The radius; defaults to one}
  \item{x0}{Location of the center of the circle on the x axis}
  \item{y0}{Location of the center of the circle on the y axis}
  \item{filled}{If TRUE, the circle is drawn with 'polygons'; if FALSE (the default), lines is used instead}
  \item{n}{The number of points to place on the circumference}
  \item{trim}{the number of rows to remove from both the start and end of the data.frame, and thereby from the circumference of the circle as well}
  \item{alpha}{Angle (in degrees) by which to rotate point coordinates. Only ever sensible if 'trim' is specified}
  \item{...}{Any further arguments to pass to lines() or polygon(), depending on the value of the 'filled' argument (see above)}

}
\value{
plots the circle as specified; also returns a data.frame with the specified information
}
\description{
Adds a circle to an existing plot
}