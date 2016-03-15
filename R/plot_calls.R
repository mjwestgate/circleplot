# plot functions - called by user

# function to draw a figure, if supplied with a data.frame or matrix
circleplot<-function(
	input,	# a distance matrix (class 'dist') or square matrix (class matrix)
	cluster=TRUE, # should points be  rearranged using hclust? Defaults to TRUE
	reduce=FALSE, # should nodes with no connections be removed?
	draw=TRUE, # should the figure be drawn?
	add=FALSE, # should this figure be added to an existing plot? 
	style="classic", # "pie" or "clock" are current alternatives
	plot.control,	# a list containing plot attributes. See ?circleplot
	...
	)
	{
	# catch errors
	if(any(c("classic", "pie", "clock")==style)==FALSE){
		warning(paste("style = '", style, "' not recognised: switched to style = 'classic'", sep=""))
		style<-"classic"}

	# test whether the object given was calculated by circleplot
	check.names<-function(x){
		test<-names(x)==c("locations", "plot.control", "line.data")
		if(length(test)==0){return(FALSE)
		}else{all(test)}}
	add.existing.plot<-class(input)=="list" & length(input)==3 & check.names(input)

	# set plot window attributes
	if(draw & class(input)=="list" & check.names(input)==FALSE){
		par(mfrow=panel.dims(length(circleplot.object$lines)))}


	# if it was, extract relevant information
	if(add.existing.plot){
		plot.options<-input$plot.control
		circleplot.object<-input$locations
		line.object<-input$line.data

	# if not, calculate (and plot) node and edge locations as usual
	}else{
		dataset<-check.inputs(input, reduce)
		plot.options<-set.plot.attributes(dataset, plot.control, reduce) # set plot attributes/defaults
		circleplot.object<-calc.circleplot(dataset, plot.options, cluster, style) # get line and point attributes

		# calculate inter-point distances
		# allows setting of pc.scale (to calculate curvature of lines relative to origin)
		point.distance<-dist(circleplot.object$points[, c("x", "y")])
		scale.distance<-point.distance-min(point.distance)
		scale.distance<-((scale.distance/max(scale.distance))*
			plot.options$line.curvature[2])+ plot.options$line.curvature[1]
		scale.distance<-as.matrix(scale.distance)
		# loop to calculate and draw lines
		line.object <-lapply(circleplot.object$lines, 
			function(a, add, circleplot.object, scale.distance, plot.options){
				line.list<-split(a, c(1:nrow(a)))
				line.list<-lapply(line.list, function(x, plot.object, distance, options){
					calc.lines(x, plot.object, distance, options)},
					plot.object=circleplot.object, distance=scale.distance, options= plot.options)			
			}, add=add, circleplot.object= circleplot.object, 
				scale.distance= scale.distance, plot.options= plot.options)
	}

	# DRAW
	if(draw){

		# this has to run within lapply, in case lists are supplied to circleplot
		invisible(lapply(line.object, function(a, add, circleplot.object, plot.options){
	
			if(add==FALSE){
				do.call(par, circleplot.object$par)
				do.call(plot, circleplot.object$plot)}
	
			# draw these lines
			invisible(lapply(a, 
				FUN=function(z, asymmetric, arrow.attr){
					draw.curves(z)
					if(asymmetric)draw.arrows(z, arrow.attr)},
				asymmetric=attr(circleplot.object, "asymmetric"), arrow.attr=plot.options$arrows))
		
			# add points or polygons, depending on style
			switch(style,
			"classic"={do.call(points, 
				as.list(circleplot.object$points[, -which(colnames(circleplot.object$points)=="labels")]))},
			"pie"={invisible(lapply(circleplot.object$polygons, function(x){do.call(polygon, x)}))},
			"clock"={
				invisible(lapply(circleplot.object$nodes, function(x){do.call(lines, x)}))
				do.call(draw.circle, plot.options$border[-which(names(plot.options$border)=="tcl")])}
			)
		
			# label points
			label.suppress.test<-is.logical(plot.options$point.labels) & length(plot.options$point.labels)==1
			if(label.suppress.test==FALSE){
				labels.list<-split(circleplot.object$labels, 1:nrow(circleplot.object$labels))
				invisible(lapply(labels.list, FUN=function(x){do.call(text, x)}))}
	
		}, add=add, circleplot.object= circleplot.object, plot.options= plot.options))
	
	if(class(input)=="list" & add.existing.plot==FALSE)par(mfrow=c(1, 1))	
	} # end if(draw)

	# return information as needed
	return(invisible(list(locations= circleplot.object, plot.control=plot.options, line.data= line.object)))
	}



# simple code to get pretty point colours
point.attr<-function(distance.matrix)
	{
	if(length(attr(distance.matrix, "Labels"))==0){
		attr(distance.matrix, "Labels")<-paste("V", c(1:attr(distance.matrix, "Size")), sep="")}
	labels<-as.character(attr(distance.matrix, "Labels"))
	color.hex<-c(RColorBrewer::brewer.pal(8, "Dark2"), 
		brewer.pal(9, "Set1"),
		brewer.pal(8, "Set2")
		)[1:length(labels)]
	point.attributes<-data.frame(
			labels= labels,
			pch=19,
			col=color.hex,
			cex=3,
			stringsAsFactors=FALSE)
	return(point.attributes)
	}