# add curved connecting lines to circleplot()
draw.curves<-function(x){
	# work out if segments are required
	segment.test<-any(length(x$col)>1 | length(x$lwd)>1)
	# plot accordingly
	if(segment.test){
		segment.list<-append(
			list(x0= x$x[1:100], x1= x$x[2:101], y0= x$y[1:100], y1= x$y[2:101]),
			x[-c(1:2, 5)])
		do.call("segments", segment.list)
	}else{do.call("lines", x[1:4])}
	}


# generate a list in which each entry is a list of line attributes, to pass to draw.curves()
get.curves<-function(
	input
	)
	{

	# calculate inter-point distances, to allow setting of pc.scale (to calculate curvature of lines relative to origin)
	point.distance<-dist(input$points[, 2:3])
	scale.distance<-point.distance-min(point.distance)
	scale.distance<-((scale.distance/max(scale.distance))*
		input$plot.control$line.curvature[2])+input$plot.control$line.curvature[1]
	scale.distance<-as.matrix(scale.distance)

	# set line colours & widths. 
	# Note that this works even for binary matrices, but is later ignored if line.gradient==FALSE
	line.cuts<-cut(input$lines$value, input$plot.control$line.breaks, 
		include.lowest=TRUE, right=TRUE, labels=FALSE)
	input$lines$colour<-input$plot.control$line.cols[line.cuts]

	# new code for setting line widths
	input$lines$lwd.max<-input$plot.control$line.widths[line.cuts]	
	input$lines$lwd.min<-input$plot.control$line.widths[line.cuts]*input$plot.control$line.expansion
	
	# add line to remove NA values if plot.control$na.control is not a list
	# this reduces the time taken to draw plots with many NA values
	if(class(input$plot.control$na.control)!="list"){
		if(any(is.na(input$lines$value))){
			input$lines<-input$lines[-which(is.na(input$lines$value)==TRUE), ]}}

	# loop to calculate lines of requisite location and colour
	# line.list<-apply(input$lines, 1, FUN=function(x, input, distance){calc.lines(x, input, distance)},
	#	input=input, distance=scale.distance)
	# for some reason, apply() fails here, while a loop works; implement a loop until this is resolved.
	line.list <-list()
	for(i in 1:nrow(input$lines)){line.list[[i]]<-calc.lines(input$lines[i, ], input, distance=scale.distance)}

	return(line.list)
	}


# function to pass to apply in get.curves() to calculate locations for each line
calc.lines<-function(lines, input, distance)
	{
	# sort out line inputs
	sp1<-as.character(lines[1])
	sp2<-as.character(lines[2])
	value<-as.numeric(lines[3])
	col<-as.character(lines[5])
	lwd.min<-as.numeric(lines[7])
	lwd.max<-as.numeric(lines[6])

	# sort out other inputs
	points<-input$points
	plot.control<-input$plot.control
	binary<-input$binary
	asymmetric<-input$asymmetric

	# sort out coords for this row
	row1<-which(points$label== sp1)
	row2<-which(points$label== sp2)
	coords<-data.frame(x= points$x[c(row1, row2)], y= points$y[c(row1, row2)])
	
	# find basic spatial info on these points
	distance.thisrun<-distance[row1, row2]
	coords.scaled<-triangle.coords(coords, distance.thisrun) # what coordinates should the curve be fit to?

	# calculate the curve that fits between these points.
	# Note that if there are an even number of points, some will pass through the intercept, causing earlier code to fail
	if(coords.scaled$y[2]>0.0001){
		apex<-curve.apex(coords, distance.thisrun)
		curve.coords<-fit.quadratic(coords.scaled)
		new.curve<-as.list(reposition.curve(curve.coords, apex))
	}else{	# i.e. if a straight line
		new.curve<-list(
			x=seq(coords$x[1], coords$x[2], length.out=101), 
 				y=seq(coords$y[1], coords$y[2], length.out=101))
	} 

	# set NA behaviour
	if(is.na(value)){
		if(is.list(plot.control$na.control)){
			new.curve<-append(new.curve, plot.control$na.control) 
			new.curve<-append(new.curve, list(direction=as.numeric(lines[4])))
			}
	}else{	# i.e. if this line is not a missing value (i.e. most cases).

	# set line widths
	lwd.range<-lwd.max-lwd.min
	if(lwd.range>0){
		# set default line widths (0-1 range) assuming expansion >0
		x<-seq(-2, 2, length.out=100)
		line.widths<-dnorm(x, mean=0, sd=0.5)
		line.widths<-line.widths-min(line.widths)
		line.widths<-line.widths/max(line.widths)
		lwd.final<-(line.widths*lwd.range)+lwd.min
	}else{lwd.final<-lwd.max}

	# ensure that curves run from their start to end point
	large.x<-which(sqrt(coords$x^2)>10^-3)
	if(length(large.x)>1){large.x<-1}
	if(large.x==1){order.test<-new.curve$x[1]-coords$x[large.x]
	}else{order.test<-new.curve$x[101]-coords$x[large.x]}
	if(sqrt(order.test^2)>10^-4){
		new.curve$x<-new.curve$x[101:1]	
		new.curve$y<-new.curve$y[101:1]}
	# if direction states that the line be reversed, do so.
	if(as.numeric(lines[4])==2){
		new.curve$x<-new.curve$x[101:1]	
		new.curve$y<-new.curve$y[101:1]}

	# set line colours
	if(binary & plot.control$line.gradient){ # for the special case where line colours are set by point colours
		# get line colours from input$points
		color1<-points$col[row1]
		color2<-points$col[row2]
		color.matrix<-col2rgb(c(color1, color2))
		color.matrix.expanded<-apply(color.matrix, 1, function(x){seq(x[1], x[2], length.out=100)})
		colours.final<-rgb(color.matrix.expanded, maxColorValue=255)
		# ensure colours are in correct order
		distance.pos<-sqrt((new.curve$x[1]-points$x[row1])^2)
		if(distance.pos>0.001){colours.final<-colours.final[100:1]}		
	}else{colours.final<-col} # in all other cases

	# export
	new.curve<-append(new.curve, list(
		col=as.character(colours.final), 
		lwd=as.numeric(lwd.final),
		direction=as.numeric(lines[4])))

	}	# end if(is.na())==F

	return(new.curve)

	} # end function



# function to determine what kind of arrowhead to draw (if any) and then draw result from get.arrows()
# note: if(asymmetric) is already called; so we only need to know whether an arrow should be drawn, and in which direction
draw.arrows<-function(x, attr){
	invisible(switch(x$direction,
		 1=={draw<-TRUE; reverse<-FALSE},
		 2=={draw<-TRUE; reverse<-TRUE},
		 3=={draw<-FALSE; reverse<-NA}))
	if(draw){
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