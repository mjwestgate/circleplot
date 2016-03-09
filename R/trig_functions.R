# trigonometric functions for circleplot

## CIRCLES

# make a circle of specified size
make.circle<-function(
	n,	# number of points, equally spaced around the edge of a circle
	alpha,	# offset angle, in degrees
	k)	# scaling value - larger for bigger circles. defaults to 1
	{
	if(missing(k))k<-1
	base.alpha<-(-90-(180/n))
	if(missing(alpha)){alpha<-base.alpha}else{alpha<-alpha+base.alpha}
	alpha<-alpha*(pi/180) # convert to radians
	# get coordinates
	theta<-(2*(pi/n)*seq(0, (n-1)))-alpha
	values<-data.frame(
		theta=theta,
		x=k*cos(theta),
		y=k*sin(theta))
	# reorder such that circle is drawn clockwise from the top
	values<-values[c(nrow(values):1), ]
	rownames(values)<-c(1:nrow(values))
	return(values)
	}


# draw a circle with specified origin, circumference and attributes
draw.circle<-function(k, x0=0, y0=0, alpha=0, filled=FALSE, n=100, trim=0, ...){
	data<-make.circle(n=n, alpha= alpha, k=k)[, 2:3]
	data$x<-data$x + x0
	data$y<-data$y + y0
	data<-rbind(data, data[1, ])
	if(trim>0){
		trim.start<-c(1:trim)
		trim.end<-c((nrow(data)-trim) : nrow(data) )
		data<-data[-c(trim.start, trim.end), ]}
	if(filled){polygon(data, ...)
	}else{lines(data, ...)}
	return(invisible(data))
	} 


# take a data.frame with cols x and y, and rotate clockwise by a given number of radians
rotate.points<-function(x, rotation){
	new.x<-(x$x * cos(rotation))-(x$y * sin(rotation))
	new.y<-(x$y * cos(rotation))+(x$x * sin(rotation))
	result<-data.frame(x=new.x, y=new.y)
	return(result)}


# add polygon to edge of circle
make.polygon<-function(x, points, options, res){ #=segment.dframe, point.dframe.total, plot.options){
	min.selector<-cbind(points$x-x$x[1], points$y-x$y[1])
	row.start<-which.min(apply(min.selector, 1, function(x){sum(x^2)})) - (res/2)
	a<-nrow(x)
	min.selector<-cbind(points$x-x$x[a], points$y-x$y[a])
	row.end<-which.min(apply(min.selector, 1, function(x){sum(x^2)})) + (res/2)
	# use this to make a polygon-type data.frame
	rows<-c(row.start:row.end); inv.rows<-c(row.end:row.start)
	poly.final<-list(
		x= c(points$x[rows], points$x.max[inv.rows]),
		y= c(points$y[rows], points$y.max[inv.rows]),
		border=NA,
		col=x$col[1])
	return(poly.final)
	}



## LINES

# calculate the attributes of a triangle linking two points on the circumference and a point bisecting them, 
	# pc.scale gives the proportion of the distance between the base line and the origin
# line linking the two points is taken to be horizontal
triangle.coords<-function(coords, pc.scale=0.5)
	{
	radius<-sqrt(coords$x[1]**2 + coords$y[1]**2)
	base.length<-sqrt((coords$x[1]-coords$x[2])**2 + (coords$y[1]-coords$y[2])**2)
	adj<-base.length/2
	adj.on.radius<-adj/radius
	if(adj.on.radius<1){
		angle1<-acos(adj.on.radius)
		opp<-tan(angle1)*adj*pc.scale
	}else{opp<-0}
	coords.adjusted<-data.frame(x=c(-adj, 0, adj), y=c(0, opp, 0))
	return(coords.adjusted)
	}


# function to calculate locations for each line
calc.lines<-function(
	x, # each 'line' to be drawn, in a list
	plot.object, # called 'circleplot.object' in function 'circleplot'
 	distance, # distances between points - affected curvature
	options, # plot.options
	pc.scale=0.5 # line curvature - check how this is supplied
	){

	# sort out coords for this row
	row1<-which(plot.object$points$labels== x$sp1)
	row2<-which(plot.object$points$labels== x$sp2)
	coords<-data.frame(x= plot.object$points$x[c(row1, row2)], y= plot.object$points$y[c(row1, row2)])

	# find basic spatial info on these points
	distance.thisrun<-distance[row1, row2]
	coords.scaled<-triangle.coords(coords, distance.thisrun) # what coordinates should the curve be fit to?

	if(coords.scaled$y[2]>0.0001){ # coords scaled is a flattened curve: if this condition not met, then a straight line.
		# calculate key points to position the parabola
		mean.point<-c(x=mean(coords$x), y=mean(coords$y))
		angle<-atan(mean.point[2]/mean.point[1])## angle between 0,0 and mean.point	
		if(mean.point[1]<0){angle<-pi + angle}
		#	angle*(180/pi)
		hyp<-as.numeric(sqrt(mean.point[1]^2+ mean.point[2]^2)*pc.scale)
		adj<-as.numeric(hyp*cos(angle))
		opp<-as.numeric(hyp*sin(angle))
		multiplier<-sign(mean.point[1])
		if(multiplier<0.001 & multiplier>-0.001)multiplier<-1
		result<-data.frame(
			x=c(mean.point[1], as.numeric(mean.point[1]-(adj*multiplier))), 
			y=c(mean.point[2], as.numeric(mean.point[2]-(opp*multiplier))))
			# note: *sign() necessary to avoid -ve x vals giving apex(s) that are outside of the circle
		rownames(result)<-c("mean", "apex")
		# adjust angle to be measured from downward vertical line, not rightwards horizontal as prev.
		final.angle<-angle +(pi*0.5)	
		if(final.angle <0) final.angle <-(2*pi)+ final.angle 
		apex<-list(angle=as.numeric(final.angle), coordinates=result)
	
		# fit a parabola to these points
		model<-lm(y~x+I(x**2), data= coords.scaled)
		curve<-data.frame(x=seq(min(coords.scaled$x), max(coords.scaled$x), length.out=101))
		curve$y<-as.numeric(predict(model, curve, se.fit=FALSE))
	
		# set apex =0,0
		curve$y<-curve$y-curve$y[51]	
	
		# rotate
		result<-data.frame(
			x=(curve$x*cos(apex$angle)) - (curve$y*sin(apex$angle)),
			y=(curve$x*sin(apex$angle)) + (curve$y*cos(apex$angle)))
	
		# adjust origin to match initial points	
		point.locs<-data.frame(
			x=c(coords$x, result$x[c(1, 101)]),
			y=c(coords$y, result$y[c(1, 101)]))
		test.distances<-as.numeric(as.matrix(dist(point.locs))[3:4, 1])
		if(test.distances[1]<test.distances[2]){subtract.row<-1}else{subtract.row<-101}
		result$x<-result$x - (result$x[subtract.row]-coords$x[1])
		result$y<-result$y - (result$y[subtract.row]-coords$y[1])

	}else{	# i.e. if a straight line
		result <-list(
			x=seq(coords$x[1], coords$x[2], length.out=101), 
			y=seq(coords$y[1], coords$y[2], length.out=101))
	} 

	# ensure that curves run from their start to end point
	first.x<-which.min(sqrt((coords$x[1]-result$x)^2))
	if(first.x>1){
		result$x<-result$x[101:1]	
		result$y<-result$y[101:1]}

	# set line colours
	if(options$line.gradient){ # for the special case where line colours are set by point colours
		# get line colours from input$points
		color1<-plot.object$points$col[row1]
		color2<-plot.object$points$col[row2]
		color.matrix<-col2rgb(c(color1, color2))
		color.matrix.expanded<-apply(color.matrix, 1, function(x){seq(x[1], x[2], length.out=100)})
		colours.final<-rgb(color.matrix.expanded, maxColorValue=255)
		# ensure colours are in correct order
		distance.pos<-sqrt((result$x[1]-plot.object$points$x[row1])^2)
		if(distance.pos>0.001){colours.final<-colours.final[100:1]}		
	}else{colours.final<-x$col} # in all other cases

	# export
	result<-append(result, x[-c(1:3)])
	if(options$line.gradient){result$col<-colours.final}
	return(result)
}


# add curved connecting lines to circleplot()
draw.curves<-function(x){
	# work out if segments are required
	segment.test<-any(length(x$col)>1 | length(x$lwd)>1)
	# plot accordingly
	if(segment.test){
		segment.list<-append(
			list(x0= x$x[1:100], x1= x$x[2:101], y0= x$y[1:100], y1= x$y[2:101]),
			x[-c(1:2)])
		segment.list<-segment.list[-which(names(segment.list)=="arrows")]
		do.call("segments", segment.list)
	}else{
		if(any(names(x)=="arrows")){x<-x[-which(names(x)=="arrows")]}
		do.call("lines", x)}
	}



### ARROWS

# generate coordinates for an arrowhead
get.arrows<-function(input, attr, reverse){
	# set some defaults
	angle<-attr$angle #20		# note this gives a total angle of 40 deg. (2*angle)
	length<-attr$length # 0.05
	centre<-c(0, 0)
	angle<-angle*(pi/180)	# assume units are in degrees, and convert to radians

	# calculate information on where arrow should be located
	arrow.pc<-attr$distance #0.8
	arrow.loc<-ceiling(length(input$x)*arrow.pc)
	arrow.locs<-c((arrow.loc-1):(arrow.loc+1))
	location<-as.numeric(c(input$x[arrow.loc], input$y[arrow.loc]))

	# calculate angle of line
	x.adj<-input$x[arrow.locs[3]]-input$x[arrow.locs[1]]
	y.adj<-input$y[arrow.locs[3]]-input$y[arrow.locs[1]]
	rotation<-atan(y.adj/x.adj)
	if(x.adj>0)rotation<-rotation+pi

	# calculate x and y coordinates of vertices
	xlim<-c(centre[1]-(length*0.5), centre[1]+(length*0.5))	
	height<-length*tan(angle)
	ylim<-c(centre[2]-height, centre[2]+height)	

	# arrange for a left-facing arrow
	arrow<-data.frame(
		x=c(xlim[1], xlim[2], xlim[2], xlim[1]),
		y=c(centre[2], ylim[1], ylim[2], centre[2]))

	# adjust position to match location
	arrow<-rotate.points(arrow, rotation)
	arrow$x<-arrow$x + as.numeric(location[1])
	arrow$y<-arrow$y + as.numeric(location[2])

	return(arrow)
	}


# function to determine what kind of arrowhead to draw (if any) and then draw result from get.arrows()
draw.arrows<-function(x, attr){
	if(x$arrows){
		if(length(x$col)>1){col.final<-x$col[ceiling(101*attr$distance)]
		}else{col.final<-x$col}
		polygon(get.arrows(x, attr, reverse), border=NA, col= col.final)}
	}

