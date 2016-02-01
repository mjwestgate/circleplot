# plot functions - called by user

# function to draw a figure, if supplied with a data.frame or matrix
circleplot<-function(
	input,	# a distance matrix (class 'dist') or square matrix (class matrix)
	cluster=TRUE, # should points be  rearranged using hclust? Defaults to TRUE, but currently always FALSE for numeric inputs
	reduce=FALSE, # should nodes with no connections be removed?
	add=FALSE, # should this figure be added to an existing plot? 
	plot.control,	# a list containing plot attributes. See ?circleplot
	style="classic", # "pie" is the alternative; "clock" might be a fun third option (that has both points and polygons); but somewhat nonsensical given that the points and polygons would be the same colour.
	...
	)
	{
	# initial processing
	dataset<-check.inputs(input, reduce)
	plot.options<-set.plot.attributes(dataset, plot.control, reduce) # set plot attributes/defaults
	circleplot.object<-calc.circleplot(dataset, plot.options, cluster, style) # get line and point attributes

	# call plot code
	par(mfrow=panel.dims(length(circleplot.object$lines)))
	invisible(lapply(circleplot.object$lines, function(x, plot.object, plot.options){

		if(add==FALSE){
			do.call(par, plot.object$par)
			do.call(plot, plot.object$plot)}
		invisible(lapply(get.curves(plot.object$points, x, plot.options), FUN=function(z, asymmetric, arrow.attr){
			draw.curves(z)
			if(asymmetric)draw.arrows(z, arrow.attr)},
			asymmetric=attr(plot.object, "asymmetric"), arrow.attr=plot.options$arrows))
	
		# add points or polygons, depending on style
		if(style=="classic"){
			do.call(points, as.list(plot.object$points[, -which(colnames(plot.object$points)=="labels")]))
		}else{
			invisible(lapply(plot.object$polygons, function(x){do.call(polygon, x)}))}
	
		# label points
		label.suppress.test<-is.logical(plot.options$point.labels) & length(plot.options$point.labels)==1
		if(label.suppress.test==FALSE){
			labels.list<-split(plot.object$labels, 1:nrow(plot.object$labels))
			invisible(lapply(labels.list, FUN=function(x){do.call(text, x)}))}
	
	}, plot.object= circleplot.object, plot.options= plot.options))
	par(mfrow=c(1, 1))

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



# add keys to a circleplot
add.key<-function (circleplot.result, labels, exclude.lines=999,  reverse=TRUE, side="right",
	cex=1, mar, ...) 
	{
	# prep
	plot.list<-get.key.dframe(circleplot.result, exclude.lines, reverse, cex)
	if(missing(labels)==FALSE){plot.list$text$labels<-labels}
	if(side=="right"){side<-4; mar.default=c(1, 1, 1, 5)
		}else{side<-2; mar.default =c(1, 5, 1, 1)}
	if(missing(mar))mar<-mar.default
	plot.list$text<-append(plot.list$text, list(side=side, cex.axis=cex))
	# draw
	par(mar=mar)
    plot(c(1) ~ c(1), ann = FALSE, axes = FALSE, type = "n", xlim =c(0, 1), ylim = c(0, 1), ...)
	line.fun<-function(x0, x1, y0, y1, ...){lines(x=c(x0, x1), y=c(y0, y1), ...)}
	invisible(lapply(split(plot.list$lines, c(1:nrow(plot.list$lines))), FUN=function(x){do.call("line.fun", x)}))
	do.call("axis", plot.list$text)
}