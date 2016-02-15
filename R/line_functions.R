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


# generate a list in which each entry is a list of line attributes, to pass to draw.curves()
get.curves<-function(
	points, # from calc.circleplot 
	lines,
	plot.options # from set.plot.attributes
	)
	{

	# calculate inter-point distances, to allow setting of pc.scale (to calculate curvature of lines relative to origin)
	point.distance<-dist(points[, c("x", "y")])
	scale.distance<-point.distance-min(point.distance)
	scale.distance<-((scale.distance/max(scale.distance))*
		plot.options$line.curvature[2])+ plot.options$line.curvature[1]
	scale.distance<-as.matrix(scale.distance)

	# loop to calculate lines of requisite location and colour
	line.list<-split(lines, c(1:nrow(lines)))
	line.list<-lapply(line.list, function(x, input, distance, plot.options){
		calc.lines(x, input, distance, plot.options)},
		input=points, distance=scale.distance, plot.options= plot.options)
	names(line.list)<-apply(lines[, 1:2], 1, function(x){paste(x, collapse="_")})
	return(line.list)
	}


# function to pass to apply in get.curves() to calculate locations for each line
calc.lines<-function(x, points, distance, plot.options)
	{
	# sort out coords for this row
	row1<-which(points$labels== x$sp1)
	row2<-which(points$labels== x$sp2)
	coords<-data.frame(x= points$x[c(row1, row2)], y= points$y[c(row1, row2)])

	# find basic spatial info on these points
	distance.thisrun<-distance[row1, row2]
	coords.scaled<-triangle.coords(coords, distance.thisrun) # what coordinates should the curve be fit to?

	# calculate the curve that fits between these points.
	if(coords.scaled$y[2]>0.0001){
		apex<-curve.apex(coords, distance.thisrun)
		curve.coords<-fit.quadratic(coords.scaled)
		new.curve<-reposition.curve(curve.coords, apex, coords)
	}else{	# i.e. if a straight line
		new.curve<-list(
			x=seq(coords$x[1], coords$x[2], length.out=101), 
			y=seq(coords$y[1], coords$y[2], length.out=101))
	} 

	# ensure that curves run from their start to end point
	first.x<-which.min(sqrt((coords$x[1]-new.curve$x)^2))
	if(first.x>1){
		new.curve$x<-new.curve$x[101:1]	
		new.curve$y<-new.curve$y[101:1]}

	# set line colours
	if(plot.options$line.gradient){ # for the special case where line colours are set by point colours
		# get line colours from input$points
		color1<-points$col[row1]
		color2<-points$col[row2]
		color.matrix<-col2rgb(c(color1, color2))
		color.matrix.expanded<-apply(color.matrix, 1, function(x){seq(x[1], x[2], length.out=100)})
		colours.final<-rgb(color.matrix.expanded, maxColorValue=255)
		# ensure colours are in correct order
		distance.pos<-sqrt((new.curve$x[1]-points$x[row1])^2)
		if(distance.pos>0.001){colours.final<-colours.final[100:1]}		
	}else{colours.final<-x$col} # in all other cases

	# export
	new.curve<-append(new.curve, x[-c(1:3)])
	if(plot.options$line.gradient){new.curve$col<-colours.final}

	return(new.curve)

	} # end function



# function to determine what kind of arrowhead to draw (if any) and then draw result from get.arrows()
draw.arrows<-function(x, attr){
	if(x$arrows){
		if(length(x$col)>1){col.final<-x$col[ceiling(101*attr$distance)]
		}else{col.final<-x$col}
		polygon(get.arrows(x, attr, reverse), border=NA, col= col.final)}
	}


# take a data.frame with cols x and y, and rotate clockwise by a given number of radians
rotate.points<-function(x, rotation){
	new.x<-(x$x * cos(rotation))-(x$y * sin(rotation))
	new.y<-(x$y * cos(rotation))+(x$x * sin(rotation))
	result<-data.frame(x=new.x, y=new.y)
	return(result)}


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