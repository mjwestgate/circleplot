\name{circleplot_fun}
\alias{circleplot}
\title{Plot a pairwise associations between nodes}
\usage{
circleplot(x, cluster, reduce, add, plot.control, type)
}
\arguments{
  \item{x}{a data source containing information on pairwise associations. Can be a data.frame with 3 columns (e.g. as returned by sppairs::spaa) or a matrix of class 'matrix' or 'dist'. Alternatively, a list containing >1 of the above.}

  \item{cluster}{logical - should points be rearranged using hclust? Defaults to TRUE, unless distance.matrix contains missing values, in which case cluster is set to FALSE and cannot be overwritten.}

  \item{reduce}{logical - should points with no connections be removed from the plot? Defaults to FALSE}

  \item{add}{logical (defaulting to FALSE), with behaviour dependent on class(x). When class(x)=="list", default is to divide the window automatically using par(mfrow), while setting add=TRUE allows you to specify your own mfrow values. When class(x)!="list", asks whether new points and lines be added to an existing circleplot.}

  \item{plot.control}{a list giving information on how the plot should appear. If given, may contain any or all of the named objects given in notes (below).}

  \item{type}{If x is a list, this argument determines which nodes are drawn on all plots. type="OR" (the default) draws all nodes present in any of the list enties; type="AND" draws only those nodes that are present in all entries.}

}
\value{
Draws a plot showing the labels of the specified object as nodes around the circumference of a circle, with parabolic lines joining them. When a list is supplied as an input, circleplot invisibly returns a list altered by clean.list(); otherwise invisibly returns point and line attributes of a single plot.
}
\description{
Draws a circular plot of pairwise association values. 
}
\note{
options for plot.control are as follows:
  \describe{
    \item{plot.rotation}{a single numeric value, used to set the number of degrees that points are rotated. Default is to arrange points clockwise from vertical.}
	\item{plot}{a list of arguments to pass to plot()}
	\item{par}{a list of arguments to pass to par()}
    \item{points}{a data.frame giving point attributes, where colnames are the names of arguments to be passed to points(), plus an extra column named 'labels' for matching to the labels of the input distance matrix.  An example can be seen by calling point.attr(x).}
    \item{point.labels}{either a logical term (defaulting to TRUE) stating whether points be labelled; or a data.frame of same nrows as 'points' (above), listing attributes of labels. If the latter, the data.frame must include one column named 'labels'; other column names should be commands to pass to text(). Note that 'offset' is an exception; rather than being passed to text, its average value is used as the absolute distance from points that labels are placed (Defaults to 0.05).}
    \item{line.gradient}{logical, default to FALSE; should lines display a colour gradient. This is usually only sensible for binary matrices (i.e. where line width or colour is not meaningful).}
    \item{line.breaks}{vector of breaks in distance.matrix used to determine line colours and widths. Note that NA values are ignored, and so are dealt with separately using na.control (below).}
    \item{line.cols}{vector containing colours, either named or hexadecimal. Used with 'line.breaks' above; length should equal either 1 or length(breaks)-1.}
    \item{line.widths}{vector used to set line widths, passed to lines(lwd) in conjunction with 'line.breaks' above. If length = 1, is the width of all lines; if length=length(breaks)-1, passed directly. Unlike earlier versions, specifying min and max widths may no longer return sensible results.}
    \item{line.expansion}{length-1 vector giving the percentage of the line width that is lost as the line approaches a vertex; i.e. one = strong tapering, zero=no tapering. }
    \item{line.curvature}{length-2 vector (or optionally, a list) that can be used to alter line straightness (i.e. the proportional curvature of the line). If given, must contain two values; the first is used to add curvature to all lines, the second multiplies the curvature by the distance between both points. Setting line.curvature[1]=0 gives boundary lines that lack any curvature, while setting both values to zero gives straight lines. Defaults to c(add=0.25, multiply=0.35), for consistency with earlier versions. Note that high values (>0.5) make the plot difficult to read, while setting high values for both attributes can cause points and lines to fail to intersect.}
    \item{arrows}{list containing up to three named arguments: length gives the length of the arrow; angle the 'sharpness' of the arrowhead (in degrees); and distance is the proportion of line length that the arrow should be placed. Defaults are arrows=list(length=0.07, angle=10, distance=0.75)}
	\item{na.control}{command to control plotting of missing line values. Should be a list of commands to pass to lines(), defaulting to list(lwd=1, lty=2, col="grey"). Note that other values (i.e. supplying NA, or any object other than a list) suppresses plotting of missing values: this can be useful, but may be misleading, especially for binary matrices.}
}
}

