# plot functions - called by user

# wrapper function
circleplot<-function(
	input,	# any of class = list, matrix, dist, data.frame
	...
	){
	if(class(input)=="list"){run.circleplot.multiple(input, ...)
	}else{run.circleplot.single(input, ...)}
	}


# function to return a sensible outcome if multiple plots are required
# goal here is to ensure that all plots have the same points on their respective circumferences
run.circleplot.multiple<-function(
	input,	# a distance matrix (class 'dist') or square matrix (class matrix)
	plot.control, 
	add=FALSE, # should this figure be added to an existing plot? 
	compare.type="OR",
	...
	){
	comparison<-calc.overlap(input)
	dataset<-clean.list(input, type= compare.type)
	n<-length(dataset)
	if(add==FALSE)par(mfrow=panel.dims(length(input)))

	if(compare.type=="OR"){
	for(i in 1:n){
		missing.rows<-which(comparison[, i]==FALSE)
		point.dframe<-data.frame(labels=rownames(comparison), col="grey30", pch=19, stringsAsFactors=FALSE)
		point.dframe$col[missing.rows]<-"grey60"

		if(missing(plot.control)){plot.control.thisrun<-list(points=point.dframe, na.control=NA)
		}else{
			plot.control.thisrun<-plot.control
			if(any(names(plot.control.thisrun)=="points")){
				point.dframe<-do.call("data.frame", append(list(labels=rownames(comparison)), plot.control.thisrun$points))
				if(any(colnames(point.dframe)=="col")){point.dframe$col[missing.rows]<-"grey60"
				}else{point.dframe$col<-"grey30"; point.dframe$col[missing.rows]<-"grey60"}
				plot.control.thisrun$points<-point.dframe
			}else{plot.control.thisrun$points<-point.dframe}
			if(any(names(plot.control.thisrun)=="na.control")==FALSE){plot.control.thisrun$na.control<-NA}
			}
		run.circleplot.single(dataset[[i]], plot.control=plot.control.thisrun, ...)
		}
	}else{ # compare.type =AND
		invisible(lapply(dataset, FUN=function(x){run.circleplot.single(x, ...)}))
		}	
	if(add==FALSE)par(mfrow=c(1, 1))
	return(invisible(dataset))
}



# function to draw a figure, if supplied with a data.frame or matrix
run.circleplot.single<-function(
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
	curve.list<-get.curves(circleplot.object, plot.options)

	# call plot code
	if(add==FALSE){
		do.call(par, circleplot.object$par)
		do.call(plot, circleplot.object$plot)}
	invisible(lapply(curve.list, FUN=function(x, asymmetric, arrow.attr){
		draw.curves(x)
		if(asymmetric)draw.arrows(x, arrow.attr)},
		asymmetric=attr(circleplot.object, "asymmetric"), arrow.attr=plot.options$arrows))

	# add points or polygons, depending on style
	if(style=="classic"){
		do.call(points, as.list(circleplot.object$points[, -which(colnames(circleplot.object$points)=="labels")]))
	}else{
		invisible(lapply(circleplot.object$polygons, function(x){do.call(polygon, x)}))}

	# label points
	label.suppress.test<-is.logical(plot.options$labels) & length(plot.options$point.labels)==1
	if(label.suppress.test==FALSE){
		labels.list<-split(circleplot.object$labels, 1:nrow(circleplot.object$labels))
		invisible(lapply(labels.list, FUN=function(x){do.call(text, x)}))}
	
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