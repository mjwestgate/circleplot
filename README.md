circleplot
==========

Tools for exploring distance and association matrices using circular plots.

# Example:
```
# SIMPLE APPLICATIONS
# get circleplot
library(devtools)
install_github('mjwestgate/circleplot')
library(circleplot)

# create some binary datasets
binary.matrix<-matrix(
  data=cut(rnorm(11**2), breaks=c(-20, 0, 20), labels=FALSE)-1,
  nrow=11, ncol=11) # asymmetric, square matrix
binary.dist<-as.dist(binary.matrix) # symmetric, lower-triangular matrix

# examples
circleplot(binary.dist) # default settings
circleplot(binary.dist, cluster=FALSE) # change point arrangement
circleplot(binary.dist, style="clock") # change style

# set point attributes and plot
point.attributes<-point.attr(binary.dist)
point.attributes$cex<-0.7
circleplot(binary.dist, style="pie", cluster=FALSE, plot.control=list(
    points=point.attributes, 
    line.gradient=TRUE))

# numeric matrices
numeric.matrix<-matrix(data=rnorm(11**2), nrow=11, ncol=11) # asymmetric, square
numeric.dist<-as.dist(numeric.matrix) # symmetric, lower-triangular

# examples
circleplot(numeric.dist, style="classic") # default
circleplot(numeric.dist-min(numeric.dist), style="clock",
    plot.control=list(border=list(lwd=2))) # version with only positive values


# NEW FEATURES AT V0.4
library(sppairs)
library(cooccur)
data(beetles)

# manually reorder points
point.order<-point.attributes
point.order$order<-c(nrow(point.order):1)
circleplot(binary.matrix, style="pie", cluster=TRUE, plot.control=list(
    arrows=list(length=0),
    points=point.order, 
    line.gradient=TRUE))

# manually specify line attributes
input<-spaa(beetles)
input$col<-"red"; input$col[which(input$sp1=="V1")]<-"blue"
circleplot(input, cluster=FALSE, style="clock")

# plot a list of datasets
data.list<-list(beetles[, 1:8], beetles[, 4:12], beetles[, 8:16], beetles[, 10:17])
names(data.list)<-paste("subset", c(1:4), sep="_")
spaa.list<-lapply(data.list, function(x){spaa(x, method="or.symmetric")})
circleplot(spaa.list, cluster=FALSE, style="clock")
```
