\name{add_key}
\alias{add.key}
\title{Draw the key to line colours for a given circleplot}
\usage{
add.key(circleplot.output, labels, exclude.lines, reverse, cex, ...)
}
\arguments{
  \item{circleplot.output}{Invisibly returned result of command circleplot()}
  \item{labels}{(optional) replacement labels for line attributes}
  \item{exclude.lines}{numeric vector giving the lines that should be excluded from the plot (if any), counted from the top down}
  \item{reverse}{if TRUE (the default), reverses the order of lines in the key, such that high values are place above low values. Note that this command applies after label placement, so both 'labels' and 'exclude.lines' commands (above) must refer to intial order.}	
  \item{cex}{character expansion factor, passed to lines()}
  \item{...}{further information passed to plot()}
}
\value{
Draws a simple key
}
\description{
Draws a new plot, giving the key to an existing circleplot. Some care with cex, xlim & ylim is often necessary for optimal display in the current version.
}
\examples{
# generate some data
input.data<-rnorm(11**2)
numeric.matrix<-matrix(data= input.data, nrow=11, ncol=11)
numeric.matrix[c(2, 5, 12, 16, 17, 21, 25, 40)]<-NA # optionally add some NA values
numeric.dist<-as.dist(numeric.matrix)

# plot 
split.screen(matrix(data=c(rep(c(0.1, 1), 2), rep(c(0.02, 0.3), 2)), nrow=2, ncol=4, byrow=TRUE))
screen(1)
plot.object<-circleplot(numeric.dist, plot.control=list(
	line.width=1,
	line.expansion=0,
	line.curvature=list(0.1, 0.3)))
screen(2)
par(mar=rep(0, 4))
add.key(plot.object, xlim=c(0, 1), cex=0.6)
close.screen(all=TRUE)
}