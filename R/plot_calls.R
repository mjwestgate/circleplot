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

	# calculate inter-point distances, to allow setting of pc.scale (to calculate curvature of lines relative to origin)
	point.distance<-dist(circleplot.object$points[, c("x", "y")])
	scale.distance<-point.distance-min(point.distance)
	scale.distance<-((scale.distance/max(scale.distance))*
		plot.options$line.curvature[2])+ plot.options$line.curvature[1]
	scale.distance<-as.matrix(scale.distance)

	# set plot window attributes
	if(add==FALSE){
		do.call(par, circleplot.object$par)
		do.call(plot, circleplot.object$plot)}
	if(class(input)=="list")par(mfrow=panel.dims(length(circleplot.object$lines)))

	# loop to calculate lines of requisite location and colour
	line.data<-circleplot.object$lines[[1]]
	line.list<-split(line.data, c(1:nrow(line.data)))
	line.list<-lapply(line.list, function(x, plot.object, distance, options){
		calc.lines(x, plot.object, distance, options)},
		plot.object=circleplot.object, distance=scale.distance, options= plot.options)

	# draw these lines
	invisible(lapply(line.list, 
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
	
	if(class(input)=="list")par(mfrow=c(1, 1))

	# return information as needed
	return(invisible(list(locations= circleplot.object, plot.control=plot.options)))
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