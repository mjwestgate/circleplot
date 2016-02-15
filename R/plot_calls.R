# plot functions - called by user

# function to draw a figure, if supplied with a data.frame or matrix
circleplot<-function(
	input,	# a distance matrix (class 'dist') or square matrix (class matrix)
	cluster=TRUE, # should points be  rearranged using hclust? Defaults to TRUE
	reduce=FALSE, # should nodes with no connections be removed?
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

	# initial processing
	dataset<-check.inputs(input, reduce)
	plot.options<-set.plot.attributes(dataset, plot.control, reduce) # set plot attributes/defaults
	circleplot.object<-calc.circleplot(dataset, plot.options, cluster, style) # get line and point attributes

	# call plot code
	if(class(input)=="list")par(mfrow=panel.dims(length(circleplot.object$lines)))
	invisible(lapply(circleplot.object$lines, function(x, plot.object, plot.options){

		# set plot window attributes
		if(add==FALSE){
			do.call(par, plot.object$par)
			do.call(plot, plot.object$plot)}

		# add lines
		invisible(lapply(get.curves(plot.object$points, x, plot.options), FUN=function(z, asymmetric, arrow.attr){
			draw.curves(z)
			if(asymmetric)draw.arrows(z, arrow.attr)},
			asymmetric=attr(plot.object, "asymmetric"), arrow.attr=plot.options$arrows))

		# add points or polygons, depending on style
		switch(style,
		"classic"={do.call(points, 
			as.list(plot.object$points[, -which(colnames(plot.object$points)=="labels")]))},
		"pie"={invisible(lapply(plot.object$polygons, function(x){do.call(polygon, x)}))},
		"clock"={
			invisible(lapply(plot.object$nodes, function(x){do.call(lines, x)}))
			do.call(draw.circle, plot.options$border[-which(names(plot.options$border)=="tcl")])}
		)
	
		# label points
		label.suppress.test<-is.logical(plot.options$point.labels) & length(plot.options$point.labels)==1
		if(label.suppress.test==FALSE){
			labels.list<-split(plot.object$labels, 1:nrow(plot.object$labels))
			invisible(lapply(labels.list, FUN=function(x){do.call(text, x)}))}
	
	}, plot.object= circleplot.object, plot.options= plot.options))
	if(class(input)=="list")par(mfrow=c(1, 1))

	# return information as needed
	return(invisible(list(locations= dataset, plot.control=plot.options)))
	}




# simple code to get pretty point colours
point.attr<-function(distance.matrix)
	{
	if(length(attr(distance.matrix, "Labels"))==0){
		attr(distance.matrix, "Labels")<-c(1:attr(distance.matrix, "Size"))}
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