# add curved connecting lines to circleplot()
draw.curves<-function(
	#dataset,
	#plot.locations,
	#plot.control
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
		input$plot.locations$lines<-input$plot.locations$lines[-which(
			is.na(input$plot.locations$lines$value)==TRUE), ]
		}

	# loop to draw lines of requisite location and colour
	for(i in 1:dim(input$lines)[1])	
		{
		# sort out coords for this row
		row1<-which(input$points$label== input$lines$sp1[i])
		row2<-which(input$points$label== input$lines$sp2[i])
		coords<-data.frame(x= input$points$x[c(row1, row2)],
			y= input$points$y[c(row1, row2)])
		
		# find basic spatial info on these points
		distance.thisrun<-scale.distance[row1, row2]
		coords.scaled<-triangle.coords(coords, distance.thisrun) # what coordinates should the curve be fit to?

		# calculate the curve that fits between these points.
		# Note that if there are an even number of points, some will pass through the intercept, causing earlier code to fail
		if(coords.scaled$y[2]>0.0001){
			apex<-curve.apex(coords, distance.thisrun)
			curve.coords<-fit.quadratic(coords.scaled)
			new.curve<-reposition.curve(curve.coords, apex)
		}else{	# i.e. if a straight line
			new.curve<-data.frame(
				x=seq(coords$x[1], coords$x[2], length.out=101), 
 				y=seq(coords$y[1], coords$y[2], length.out=101))
		} 

		# set NA behaviour
		if(is.na(input$lines$value[i])){
			if(is.list(input$plot.control$na.control)){
				na.plot<-list(x=new.curve$x, y=new.curve$y)
				na.plot<-append(na.plot, input$plot.control$na.control)
				do.call("lines", na.plot)
				}
		}else{	# i.e. if this line is not a missing value (i.e. most cases).

		# set line widths
		lwd.range<-input$lines$lwd.max[i]-input$lines$lwd.min[i]
		line.widths.thisrun<-(line.widths*lwd.range)+ input$lines$lwd.min[i]

		# set line colours according to categorical or continuous lines 	
		if(input$binary){

			if(input$asymmetric){
				color.matrix<-col2rgb(input$plot.control$line.cols)
				color.matrix.expanded<-apply(color.matrix, 1, function(x){seq(x[1], x[2], length.out=30)})
				colours.final<-rgb(color.matrix.expanded, maxColorValue=255)
				colours.final<-c(rep(colours.final[1], 50), colours.final, rep(colours.final[30], 20))
				# ensure colours are in correct order
				distance.pos<-sqrt((new.curve$x[1]-input$plot.locations$points$x[row1])^2)
				if(distance.pos>0.001){colours.final<-colours.final[100:1]}
				# reverse if order is wrong
				if(input$lines$value[i]==-1){colours.final<-colours.final[100:1]}		
				if(input$lines$value[i]==2){colours.final<-rep(input$plot.control$line.cols[2], 100)}

			}else{	# symmetric

				if(input$plot.control$line.gradient){		# lines coloured according to a gradient
					# get line colours from input$points
					color1<-input$points$col[row1]
					color2<-input$points$col[row2]
					color.matrix<-col2rgb(c(color1, color2))
					color.matrix.expanded<-apply(color.matrix, 1, function(x){seq(x[1], x[2], length.out=100)})
					colours.final<-rgb(color.matrix.expanded, maxColorValue=255)
					# ensure colours are in correct order
					distance.pos<-sqrt((new.curve$x[1]-input$points$x[row1])^2)
					if(distance.pos>0.001){colours.final<-colours.final[100:1]}		

				}else{
					colours.final<-rep(input$lines$colour[i], 100)	# single colour

				}}	# end if asymmetric
		}else{	# if numeric
			colours.final<-rep(input$lines$colour[i], 100)
		}

		# draw a line that smoothly changes between these colours
		segments(
			x0= new.curve$x[1:100], x1= new.curve$x[2:101],
			y0= new.curve$y[1:100], y1= new.curve$y[2:101],
			col= colours.final,
			lwd= line.widths.thisrun)

		}	# end if(is.na())==F
		}	# end loop
	}	# end function
