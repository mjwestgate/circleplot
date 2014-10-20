circleplot
==========

Tools for exploring distance and association matrices using circular plots. Support for square matrices (to allow display of directional or asymmetric relationships) is in development, but is currently not functional.

# Example:
```
# get circleplot
library(devtools)
install_github('circleplot', 'mjwestgate')
library(circleplot)

# create an example:
# a continuous matrix
test.points<-matrix(data=rnorm(22), nrow=11, ncol=2)
test.dist<-as.dist(scale(dist(test.points)))

# make binary matrix from this continuous matrix, for example purposes only
binary.vector<-cut(as.vector(test.dist), breaks=c(-20, 0, 20), labels=FALSE)-1
test.dist2<-matrix(data=rep(binary.vector, 2), 11, 11)
	rownames(test.dist2)<-c(1:11)
	colnames(test.dist2)<-c(1:11)
test.dist2<-as.dist(test.dist2)

# set point colours for either matrix (as both have same dimensions)
point.attributes<-point.attr(test.dist)



# draw some examples
par(mfrow=c(2, 2))  # set window

# simple plot of a binary matrix
circleplot(test.dist2, plot.control=list(line.curvature=0, line.width=1))

# lines set a gradient in point colours
circleplot(test.dist2, plot.control=list(
	points=point.attributes,
	line.gradient=TRUE,
	line.curvature=0.6,
	line.width=3))

# without curvature
circleplot(test.dist, plot.control=list(
	line.breaks=c(-10, -1, -0.5, 0, 0.5, 1, 10),
	line.cols=brewer.pal(6, "RdBu"),
	line.curvature=0,
	line.width=c(0.5, 2)))

# a continuous plot with curvature
circleplot(test.dist, plot.control=list(
	points=point.attributes,
	line.breaks=c(-10, -1, -0.5, 0, 0.5, 1, 10),
	line.cols=brewer.pal(6, "RdBu"),
	line.curvature=0.8,
	line.width=c(1, 5)))

par(mfrow=c(1, 1))
```
