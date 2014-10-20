# plot functions

# add curved connecting lines to circleplot()
draw.curves<-function(
	input,
	plot.control
	)
	{
	# calculate inter-point distances, to allow setting of pc.scale (to calculate curvature of lines relative to origin)
	point.distance<-dist(input$points[, 1:2])
	scale.distance<-point.distance-min(point.distance)
		multiplier<-0.35; add<-0.25
	scale.distance<-((scale.distance/max(scale.distance))*multiplier)+add
	scale.distance<-as.matrix(scale.distance)

	# set line colours. Note that this works even for binary matrices, but is later ignored if line.gradient==FALSE
	line.cuts<-cut(input$lines$value, plot.control$line.breaks, include.lowest=TRUE, labels=FALSE)
	input$lines$colour<-plot.control$line.cols[line.cuts]

	# add min and max widths per line
	if(plot.control$properties[1]){	# binary
		if(length(plot.control$line.width)==2){plot.control$line.width<-plot.control$line.width[2]}} # fix if too many vals
	if(length(plot.control$line.width)==1){	# for a single value, make the line width a maximum value
		input$lines$lwd.min<-plot.control$line.width-(plot.control$line.width*plot.control$line.curvature)
		input$lines$lwd.max<-plot.control$line.width
	}else{	# otherwise, set range
		data.thisrun<-input$lines$value	# export data on the value of each line
		specified.range<-max(plot.control$line.width)-min(plot.control$line.width)	# range of desired values
		data.thisrun<-data.thisrun-min(data.thisrun)	# scale data.this run to this same range
		data.thisrun<-(data.thisrun/max(data.thisrun))*specified.range
		input$lines$lwd.min<-data.thisrun-(data.thisrun*plot.control$line.curvature)+min(plot.control$line.width)
		input$lines$lwd.max<-data.thisrun+min(plot.control$line.width)
		}

	# set default line widths (0-1 range)
	x<-seq(-2, 2, length.out=100)
	line.widths<-dnorm(x, mean=0, sd=0.5)
	line.widths<-line.widths-min(line.widths); line.widths<-line.widths/max(line.widths)

	# loop to draw lines of requisite location and colour
	for(i in 1:dim(input$lines)[1])	
		{
		# sort out coords for this row
		row1<-which(input$points$label==input$lines$sp1[i])
		row2<-which(input$points$label==input$lines$sp2[i])
		coords<-data.frame(x=input$points$x[c(row1, row2)],
			y=input$points$y[c(row1, row2)])
		
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
		lwd.range<-input$lines$lwd.max[i]-input$lines$lwd.min[i]
		line.widths.thisrun<-(line.widths*lwd.range)+input$lines$lwd.min[i]

		# set line colours according to categorical or continuous lines 	
		if(plot.control$properties[1]){	# binary

			if(plot.control$properties[2]){ # asymmetric
				color.matrix<-col2rgb(plot.control$line.cols)
				color.matrix.expanded<-apply(color.matrix, 1, function(x){seq(x[1], x[2], length.out=50)})
				colours.final<-rgb(color.matrix.expanded, maxColorValue=255)
				colours.final<-c(rep(colours.final[1], 50), colours.final)
				# ensure colours are in correct order
				distance.pos<-sqrt((new.curve$x[1]-input$points$x[row1])^2)
				if(distance.pos>0.001){colours.final<-colours.final[100:1]}
				# reverse if order is wrong
				if(result$lines$value[i]==-1){colours.final<-colours.final[100:1]}		
				if(result$lines$value[i]==2){colours.final<-rep(plot.control$line.cols[2], 100)}

			}else{	# symmetric

				if(plot.control$line.gradient){		# lines coloured according to a gradient
					# get line colours from input$points
					color1<-input$points$colour[row1]
					color2<-input$points$colour[row2]
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
		}	# end loop
	}	# end function



# function to set plot properties
check.plot.control<-function(
	distance.matrix,
	plot.control
	)
	{
# determine properties of input 
	input<-distance.matrix

	# work out if input is binary or continuous
	binary.test<-c(max(distance.matrix, na.rm=TRUE)-min(distance.matrix, na.rm=TRUE)==1,	
		max(distance.matrix, na.rm=TRUE)==1)
	if(any(binary.test==FALSE)==FALSE){binary.test<-TRUE}else{binary.test<-FALSE}

	# check whether the input matrix is symmetric or asymmetric
	#distance.matrix<-as.dist(distance.matrix)
	if(binary.test){
		dist1<-as.dist(2-(distance.matrix +t(distance.matrix)))	
		dist2<-as.dist(2-(2* distance.matrix))
		asymmetry.test<-any(c(dist1==dist2)==FALSE)
		if(asymmetry.test){distance.matrix<-dist1
		}else{distance.matrix<-dist2}
	}else{
		dist1<-as.dist(as.matrix(distance.matrix))
		dist2<-as.dist(t(as.matrix(distance.matrix)))
		asymmetry.test<-any(c(dist1==dist2)==FALSE)
		if(asymmetry.test){
			dist1[1:length(dist1)]<-apply(cbind(as.vector(dist1), as.vector(dist2)), 1, mean)}
		distance.matrix<-dist1
		}

	# export these
	matrix.properties<-c(
		#class=class(distance.matrix), 
		binary=binary.test,
		asymmetric= asymmetry.test)

	# if there are now row or column headings, add these now
	if(length(attr(distance.matrix, "Labels"))==0){
		attr(distance.matrix, "Labels")<-c(1:attr(distance.matrix, "Size"))}

	# generate a some default values for points
	point.defaults<-data.frame(
			label=attr(distance.matrix, "Labels"),
			colour=rep(rgb(t(col2rgb("grey30")), maxColorValue=255), attr(distance.matrix, "Size")),
			size=rep(2, attr(distance.matrix, "Size")),
			stringsAsFactors=FALSE)
	rownames(point.defaults)<-point.defaults$label

	# generate sensible cuts for line colours
	if(binary.test){cut.vals<-c(-1, 2)
	}else{cut.vals<-c(min(distance.matrix, na.rm=TRUE), max(distance.matrix, na.rm=TRUE))}

	# make a list of point and line attributes, showing the required properties
	plot.defaults<-list(
		input.matrix=input,
		processed.matrix=distance.matrix,
		properties=matrix.properties,
		points=point.defaults,
		line.gradient=FALSE,	# option for binary matrices only
		line.breaks=cut.vals,
		line.cols="grey30",
		line.curvature=0.3,
		line.width=1
		)
	
	# overwrite these values where others are provided
	if(missing(plot.control)==FALSE){
		names.provided<-names(plot.control)
		for(i in 4:9){
			if(any(names.provided==names(plot.defaults)[i])){
			entry.thisrun<-which(names.provided==names(plot.defaults)[i])
			plot.defaults[[i]]<-plot.control[[entry.thisrun]]
			}}}

	# make up to two colours if asymmetry.test==TRUE, and two points have not yet been provided
	default.directional.cols<-c("grey80", "grey10")
	if(asymmetry.test){
		if(length(plot.defaults$line.cols)==1){
			if(plot.defaults$line.cols=="grey30"){plot.defaults$line.cols<-default.directional.cols
			}else{plot.defaults$line.cols<-c(default.directional.cols[1], plot.defaults$line.cols)}}}

	return(plot.defaults)
	}	# end function
	


# function to use the above code to draw a figure
circleplot<-function(
	distance.matrix,	# i.e. a distance matrix (class 'dist') containing binary values
	plot.control	# a matrix containing any or all of the following compenents:
		# points = point.attributes, # if given, a data.frame with colnames=c('label', "colour", "size")
		# lines 	
			# .gradient - TRUE or FALSE - used to determine whether lines should display a colour gradient
			# .breaks - vector used to determine the breaks for colours
			# .colours - vector containing colours. used with 'breaks' above. length= length(breaks)-1
			# .curvature - a percentage giving the % of maximum sent to gaussian curve
				# i.e. curvature=1 always has a min=0, max=max, while curvature=0 is equivalent to linear
				# (formerly line.emphasis - "gaussian" or "linear").
			# .width = vector if length-1 contains absolute value; if length-2 contains minimum and max
	# simple	# later, will be passed to prep.binary - whether to draw a complex plot (or not). defaults to TRUE.
	)
	{
	# set plot attributes/defaults
	plot.control<-check.plot.control(distance.matrix, plot.control)

	# run appropriate prep code
	if(plot.control$properties[1]){	# if binary
		result<-prep.binary(distance.matrix, plot.control$points)
	}else{
		result<-prep.numeric(distance.matrix, plot.control$points)}

	# call plot code
	par(mar=rep(0.5, 4))	# set window attributes
	plot(x= result$points$x, y= result$points$y, type="n", ann=FALSE, axes=FALSE, asp=1)	# plot
	draw.curves(result, plot.control) #$points, line.list, line.widths, line.gradient)	# add lines
	points(result$points$x, result$points$y, 	# add points
		pch=19, 
		col= result$points$colour, 
		cex= result$points$size)
	text(result$points$x, result$points$y, label= result$points$label, col="white", cex=0.7)	# label points

	# if(singletons)add.outer.points
	}



# simple code to get pretty point colours
point.attr<-function(distance.matrix)
	{
	#library(RColorBrewer)	# to choose an existing palette
	labels<-as.character(attr(distance.matrix, "Labels"))
	color.hex<-c(RColorBrewer::brewer.pal(8, "Dark2"), 
		brewer.pal(9, "Set1"),
		brewer.pal(8, "Set2")
		)[1:length(labels)]
	point.attributes<-data.frame(
			label= labels,
			colour=color.hex,
			size=rep(3, length(labels)),
			stringsAsFactors=FALSE)
	}