\name{circleplot_fun}
\alias{circleplot}
\title{Plot a distance matrix}
\usage{
circleplot(distance.matrix, cluster, reduce, plot.control, ...)
}
\arguments{
  \item{distance.matrix}{a distance matrix (class dist) containing either continuous or binary data}

  \item{cluster}{logical - should points be rearranged using hclust? Defaults to TRUE, unless distance.matrix contains missing values, in which case cluster is set to FALSE and cannot be overwritten.}

  \item{reduce}{logical - should points with no connections be removed from the plot? Defaults to TRUE}

  \item{plot.control}{a list giving information on how the plot should appear. If given, may contain any or all of the named objects given in notes (below).}

  \item{...}{addtional arguments sent to par, for setting of global parameters; e.g. mar, cex, etc.}
}
\value{
Draws a plot showing the labels of the distance matrix as nodes around the circumference of a circle, with quadratic lines joining connected nodes. Invisibly returns point and line attributes.
}
\description{
Draws a circular plot of a distance matrix. 
}
\note{
options for plot.control are as follows:
  \describe{
    \item{plot.rotation}{a single numeric value, used to set the number of degrees that points are rotated. Default is to arrange points clockwise from vertical (note this is altered from the original default of 22.91 degrees (0.4 radians)).}
	\item{plot}{a list of commands to pass to plot()}
	\item{par}{a list of commands to pass to par()}
    \item{points}{a data.frame giving point attributes, where colnames are the names of arguments to be passed to points(), plus an extra column named 'labels' for matching to the labels of the input distance matrix.  An example can be seen by calling point.attr().}
    \item{point.labels}{either a logical term (defaulting to TRUE) stating whether points be labelled; or a data.frame of same nrows as 'points' (above), listing attributes of labels. If the latter, the data.frame must include one column named 'labels'; other column names should be commands to pass to text(). Note that 'offset' is an exception; rather than being passed to text, its' average value is used as the absolute distance from points that labels placed.}
    \item{line.gradient}{logical, default to FALSE; should lines display a colour gradient. This is usually only sensible for binary matrices (i.e. where line width or colour is not meaningful).}
    \item{line.breaks}{vector of breaks in distance.matrix used to determine line colours and widths. Note that NA values are ignored, and so are dealt with separately using na.control (below).}
    \item{line.cols}{vector containing colours, either named or hexadecimal. Used with 'line.breaks' above; length should equal length(breaks)-1.}
    \item{line.widths}{vector used to set line widths, passed to lines(lwd) in conjunction with 'line.breaks' above. If length-1, is the width of all lines, if length=length(breaks)-1, passed directly. For any other number of values, the maximum is chosen for all lines.}
    \item{line.expansion}{length-1 vector giving the percentage of the line width that is lost as the line approaches a vertex; i.e. one = strong tapering, zero=no tapering. }
    \item{line.curvature}{A length-2 vector (or optionally, a list) that can be used to alter line straightness (i.e. the proportional curvature of the line). If given, must contain two values; the first is used to add curvature to all lines, the second multiplies the curvature by the distance between both points. Setting line.curvature[1]=0 gives boundary lines that lack any curvature, while setting both values to zero gives straight lines. Defaults to c(add=0.25, multiply=0.35), for consistency with earlier versions. Note that high values (>0.5) make the plot difficult to read, while setting high values for both attributes can cause points and lines to fail to intersect.}
	\item{na.control}{command to control plotting of missing line values. Should be a list of commands to pass to lines(), defaulting to list(lwd=1, lty=2, col="grey"). Note that other values (i.e. supplying NA, or any object other than a list) suppresses plotting of missing values: this can be useful, but may be misleading, especially for binary matrices.}
}
}

