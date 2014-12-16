\name{add_key}
\alias{add.key}
\title{Draw the key to line colours for a given circleplot}
\usage{
add.key(circleplot.output)
}
\arguments{
  \item{circleplot.output}{the invisibly returned result of command circleplot()}
}
\value{
Draws a simple key
}
\description{
This is a simple function, and only returns line colours (and not widths) in the current version. Some messing with the cex and xlim controls is often necessary for optimal display.
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