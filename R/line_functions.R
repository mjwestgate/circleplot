# add curved connecting lines to circleplot()
draw.curves<-function(x){
	# work out if segments are required
	segment.test<-any(length(x$col)>1 | length(x$lwd)>1)
	# plot accordingly
	if(segment.test){
		segment.list<-append(
			list(x0= x$x[1:100], x1= x$x[2:101], y0= x$y[1:100], y1= x$y[2:101]),
			x[-c(1:2)])
		do.call("segments", segments.list)
	}else{do.call("lines", x)}
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
	line.cuts<-cut(input$lines$value, input$plot.control$line.breaks, include.lowest=TRUE, labels=FALSE)
	input$lines$colour<-input$plot.control$line.cols[line.cuts]

	# new code for setting line widths
	input$lines$lwd.max<-input$plot.control$line.widths[line.cuts]	
	input$lines$lwd.min<-input$plot.control$line.widths[line.cuts]*input$plot.control$line.expansion
	
	# set default line widths (0-1 range) assuming expansion >0
	x<-seq(-2, 2, length.out=100)
	line.widths<-dnorm(x, mean=0, sd=0.5)
	line.widths<-line.widths-min(line.widths); line.widths<-line.widths/max(line.widths)

	# add line to remove NA values if plot.control$na.control is not a list
	# this reduces the time taken to draw plots with many NA values
	if(class(input$plot.control$na.control)!="list"){
		input$lines<-input$lines[-which(is.na(input$lines$value)==TRUE), ]}

	
	# loop to calculate lines of requisite location and colour
	line.list<-apply(input$lines, 1, FUN=function(x, input, distance){calc.lines(x, input, distance)},
		input=input, distance=scale.distance)

	return(line.list)
	}


# function to pass to apply in get.curves() to calculate locations for each line
calc.lines<-function(lines, input, distance)
	{
	# sort out line inputs
	sp1<-as.character(lines[1])
	sp2<-as.character(lines[2])
	value<-as.numeric(lines[3])
	col<-as.character(lines[4])
	lwd.min<-as.numeric(lines[6])
	lwd.max<-as.numeric(lines[5])

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
	if(is.na(lines[3])){
		if(is.list(plot.control$na.control)){
			#na.plot<-list(x=new.curve$x, y=new.curve$y)
			new.curve<-append(new.curve, plot.control$na.control)
			#do.call("lines", na.plot)
			}
	}else{	# i.e. if this line is not a missing value (i.e. most cases).

	# set line widths
	lwd.range<-lwd.max-lwd.min
	line.widths.thisrun<-(line.widths*lwd.range)+lwd.min
	if(length(unique(line.widths.thisrun))==1){lwd.final<-line.widths.thisrun[1]
	}else{lwd.final<-line.widths.thisrun}

	# set line colours according to categorical or continuous lines 	
	if(binary){

		if(asymmetric){
			color.matrix<-col2rgb(plot.control$line.cols)
			color.matrix.expanded<-apply(color.matrix, 1, function(x){seq(x[1], x[2], length.out=30)})
			colours.final<-rgb(color.matrix.expanded, maxColorValue=255)
			colours.final<-c(rep(colours.final[1], 50), colours.final, rep(colours.final[30], 20))
			# ensure colours are in correct order
			distance.pos<-sqrt((new.curve$x[1]-points$x[row1])^2)
			if(distance.pos>0.001){colours.final<-colours.final[100:1]}
			# reverse if order is wrong
			if(lines[3]==-1){colours.final<-colours.final[100:1]}		
			if(lines[3]==2){colours.final<-rep(plot.control$line.cols[2], 100)}

		}else{	# symmetric

			if(plot.control$line.gradient){		# lines coloured according to a gradient
				# get line colours from input$points
				color1<-points$col[row1]
				color2<-points$col[row2]
				color.matrix<-col2rgb(c(color1, color2))
				color.matrix.expanded<-apply(color.matrix, 1, function(x){seq(x[1], x[2], length.out=100)})
				colours.final<-rgb(color.matrix.expanded, maxColorValue=255)
				# ensure colours are in correct order
				distance.pos<-sqrt((new.curve$x[1]-points$x[row1])^2)
				if(distance.pos>0.001){colours.final<-colours.final[100:1]}		

			}else{colours.final<-col	# single colour
			}}	# end if asymmetric
	}else{colours.final<-col} # if numeric

	new.curve<-append(new.curve, list(
		col=as.character(colours.final), 
		lwd=as.numeric(lwd.final)))

	}	# end if(is.na())==F

	return(new.curve)

	} # end function