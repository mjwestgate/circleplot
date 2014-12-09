circleplot
==========

Tools for exploring distance and association matrices using circular plots. Also allows plotting of binary directional matrices.

# Example:
```
# get circleplot
library(devtools)
install_github('circleplot', 'mjwestgate')
library(circleplot)

# example plots using fake data

# create a binary matrix
# asymmetric, square matrix
binary.matrix<-matrix(
  data=cut(rnorm(11**2), breaks=c(-20, 0, 20), labels=FALSE)-1,
  nrow=11, ncol=11)
# symmetric, lower-triangular matrix
binary.dist<-as.dist(binary.matrix)

# numeric matrices
# asymmetric, square
numeric.matrix<-matrix(data=rnorm(11**2), nrow=11, ncol=11)
# symmetric, lower-triangular
numeric.dist<-as.dist(numeric.matrix)

# set point colours for any of the above matrice (as all have same dimensions)
# library(RColorBrewer)
point.attributes<-point.attr(binary.dist)

# test binary presentation
circleplot(binary.dist)	# default grey
circleplot(binary.dist, plot.control=list(points=point.attributes))	# change point colours only
circleplot(binary.dist, plot.control=list(line.cols="blue"))	# change line colours only
circleplot(binary.dist, plot.control=list(
	line.width=3, line.curvature=0.7))	# change line appearance only
circleplot(binary.dist, plot.control=list(
	points=point.attributes, line.gradient=TRUE))	# line colours blend between point colours

# test binary asymmetric presentation
circleplot(binary.matrix)	# default: direction indicated by change from light to dark grey
circleplot(binary.matrix, plot.control=list(line.cols="blue"))	# change arrow end of the line
circleplot(binary.matrix, plot.control=list(line.cols=c("lightsteelblue","black")))	# change both line cols

# test numeric matrix presentation
circleplot(numeric.dist) # default settings if min<0<max
circleplot(numeric.dist-min(numeric.dist)) # default if min>0
circleplot(numeric.dist, plot.control=list(
	line.breaks=c(-20, -1, -0.5, 0, 0.5, 1, 20),
	line.cols=brewer.pal(6, "PuOr")[6:1]))	# set own breaks and colour palette
circleplot(numeric.dist, plot.control=list(line.width=c(1, 3))) # set larger effect sizes to have thicker lines
circleplot(numeric.dist, plot.control=list(
	line.width=c(1, 4),
	line.curvature=0.8)) # add curves to above example

# test a numeric asymmetric matrix
circleplot(numeric.matrix)	# intentionally fails - code incomplete as yet


# draw the best of the above
quartz(width=5, height=5)
par(mfrow=c(2, 2))

# binary symmetric
circleplot(binary.dist, plot.control=list(
	points=point.attributes, line.gradient=TRUE,
	line.width=3, line.curvature=0.7))
# binary asymmetric/directional
circleplot(binary.matrix, plot.control=list(line.cols=c("lightsteelblue","black"),
	line.width=3, line.curvature=0.7))
# +ve only numeric matrix
circleplot(numeric.dist-min(numeric.dist),
	plot.control=list(line.width=c(1, 3), line.curvature=0.5))
# diverging numeric matrix
circleplot(numeric.dist, plot.control=list(
	line.width=c(1, 3), line.curvature=0.5))

par(mfrow=c(1, 1))
```
