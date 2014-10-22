# add curved connecting lines to circleplot()
draw.curves<-function(
	dataset,
	plot.locations,
	plot.control
	)
	{
	# calculate inter-point distances, to allow setting of pc.scale (to calculate curvature of lines relative to origin)
	point.distance<-dist(plot.locations$points[, 1:2])
	scale.distance<-point.distance-min(point.distance)
		multiplier<-0.35; add<-0.25
	scale.distance<-((scale.distance/max(scale.distance))*multiplier)+add
	scale.distance<-as.matrix(scale.distance)

	# set line colours. Note that this works even for binary matrices, but is later ignored if line.gradient==FALSE
	line.cuts<-cut(plot.locations$lines$value, plot.control$line.breaks, include.lowest=TRUE, labels=FALSE)
	plot.locations$lines$colour<-plot.control$line.cols[line.cuts]

	# add min and max widths per line
	if(dataset$binary[1]){	# binary
		if(length(plot.control$line.width)==2){plot.control$line.width<-plot.control$line.width[2]}} # fix if too many vals
	if(length(plot.control$line.width)==1){	# for a single value, make the line width a maximum value
		plot.locations$lines$lwd.min<-plot.control$line.width-(plot.control$line.width*plot.control$line.curvature)
		plot.locations$lines$lwd.max<-plot.control$line.width
	}else{	# otherwise, set range
		data.thisrun<-plot.locations$lines$value	# export data on the value of each line
		specified.range<-max(plot.control$line.width)-min(plot.control$line.width)	# range of desired values
		data.thisrun<-data.thisrun-min(data.thisrun)	# scale data.this run to this same range
		data.thisrun<-(data.thisrun/max(data.thisrun))*specified.range
		plot.locations$lines$lwd.min<-data.thisrun-(data.thisrun*plot.control$line.curvature)+min(plot.control$line.width)
		plot.locations$lines$lwd.max<-data.thisrun+min(plot.control$line.width)
		}

	# set default line widths (0-1 range)
	x<-seq(-2, 2, length.out=100)
	line.widths<-dnorm(x, mean=0, sd=0.5)
	line.widths<-line.widths-min(line.widths); line.widths<-line.widths/max(line.widths)

	# loop to draw lines of requisite location and colour
	for(i in 1:dim(plot.locations$lines)[1])	
		{
		# sort out coords for this row
		row1<-which(plot.locations$points$label== plot.locations$lines$sp1[i])
		row2<-which(plot.locations$points$label== plot.locations$lines$sp2[i])
		coords<-data.frame(x= plot.locations$points$x[c(row1, row2)],
			y= plot.locations$points$y[c(row1, row2)])
		
		# find basic spatial info on these points
		distance.thisrun<-scale.distance[row1, row2]
		coords.scaled<-triangle.coords(coords, distance.thisrun) # what coordinates should the curve be fit to?

		# calculate the curve that fits between these points.
		# Note that if there are an even number of points, some will pass through the intercept, causing code to fail
		if(coords.scaled$y[2]>0.0001){
			apex<-curve.apex(coords, distance.thisrun)
			curve.coords<-fit.quadratic(coords.scaled)
			new.curve<-reposition.curve(curve.coords, apex)
		}else{	# i.e. if a straight line
			new.curve<-data.frame(
				x=seq(coords$x[1], coords$x[2], length.out=101), 
 				y=seq(coords$y[1], coords$y[2], length.out=101))
		} 

		# set line widths
		lwd.range<-plot.locations$lines$lwd.max[i]-plot.locations$lines$lwd.min[i]
		line.widths.thisrun<-(line.widths*lwd.range)+ plot.locations$lines$lwd.min[i]

		# set line colours according to categorical or continuous lines 	
		if(dataset$binary){

			if(dataset$asymmetric){
				color.matrix<-col2rgb(plot.control$line.cols)
				color.matrix.expanded<-apply(color.matrix, 1, function(x){seq(x[1], x[2], length.out=30)})
				colours.final<-rgb(color.matrix.expanded, maxColorValue=255)
				colours.final<-c(rep(colours.final[1], 50), colours.final, rep(colours.final[30], 20))
				# ensure colours are in correct order
				distance.pos<-sqrt((new.curve$x[1]-plot.locations$points$x[row1])^2)
				if(distance.pos>0.001){colours.final<-colours.final[100:1]}
				# reverse if order is wrong
				if(plot.locations$lines$value[i]==-1){colours.final<-colours.final[100:1]}		
				if(plot.locations$lines$value[i]==2){colours.final<-rep(plot.control$line.cols[2], 100)}

			}else{	# symmetric

				if(plot.control$line.gradient){		# lines coloured according to a gradient
					# get line colours from input$points
					color1<-plot.locations$points$colour[row1]
					color2<-plot.locations$points$colour[row2]
					color.matrix<-col2rgb(c(color1, color2))
					color.matrix.expanded<-apply(color.matrix, 1, function(x){seq(x[1], x[2], length.out=100)})
					colours.final<-rgb(color.matrix.expanded, maxColorValue=255)
					# ensure colours are in correct order
					distance.pos<-sqrt((new.curve$x[1]-plot.locations$points$x[row1])^2)
					if(distance.pos>0.001){colours.final<-colours.final[100:1]}		

				}else{
					colours.final<-rep(plot.locations$lines$colour[i], 100)	# single colour

				}}	# end if asymmetric
		}else{	# if numeric
			colours.final<-rep(plot.locations$lines$colour[i], 100)
		}

		# draw a line that smoothly changes between these colours
		segments(
			x0= new.curve$x[1:100], x1= new.curve$x[2:101],
			y0= new.curve$y[1:100], y1= new.curve$y[2:101],
			col= colours.final,
			lwd= line.widths.thisrun)
		}	# end loop
	}	# end function
