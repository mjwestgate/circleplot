# plot functions - called by user

# function to use the above code to draw a figure
circleplot<-function(
	input,	# a distance matrix (class 'dist') or square matrix (class matrix)
	cluster=TRUE, # should points be  rearranged using hclust? Defaults to true
	reduce=TRUE, # should points with no connections be removed?
	plot.control,	# a matrix containing plot attributes. See ?circleplot
	mar,
	...	# passed to par()
	)
	{
	if(missing(mar))mar<-rep(0.5, 4)

	# initial processing
	dataset<-check.inputs(input, reduce)
	plot.control<-set.plot.attributes(dataset, plot.control) # set plot attributes/defaults

	# run appropriate prep code
	if(dataset$binary){	# if binary
		result<-prep.binary(dataset, plot.control, cluster)
	}else{
		result<-prep.numeric(dataset, plot.control, cluster)}

	# determine margins
	x.lim<-c(min(result$points$x), max(result$points$x))
	# extra x margins added
	label.suppress.test<-is.logical(plot.control$point.labels) & 	length(plot.control$point.labels)==1
	# note this works beacuse set.plot.attributes allows FALSE as the only logical operator to point.labels
	if(label.suppress.test==FALSE){
		max.label<-max(nchar(result$points$labels))
		x.expansion<-max.label*0.03
		x.lim<-colSums(rbind(x.lim, c(-x.expansion, x.expansion)))}

	# call plot code
	par(mar=mar, ...)	# set window attributes
	plot(x= result$points$x, y= result$points$y, 
		xlim=x.lim,
		type="n", ann=FALSE, axes=FALSE, asp=1)	# plot
	draw.curves(dataset, result, plot.control) 	# add lines
	do.call(points, as.list(result$points[, -1]))
	
	# label points
	if(label.suppress.test==FALSE){
		labels.list<-split(plot.control$point.labels, 1:nrow(plot.control$point.labels))
		invisible(lapply(labels.list, FUN=function(x){do.call(text, x)}))}
	
	# return information as needed
	output<-list(locations=result, plot.control=plot.control)
	return(invisible(output))
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
add.key<-function(circleplot.result, 
	xlim,
	cex
	)
	{
	# set inputs
	breaks<-circleplot.result$plot.control$line.breaks
	colours<-circleplot.result$plot.control$line.cols
	widths<-circleplot.result$plot.control$line.widths
	if(missing(xlim))xlim<-c(0.4, 1)
	if(missing(cex))cex<-1

	# work out what to do with missing values
	if(any(is.na(circleplot.result$locations$lines))){
		if(is.list(circleplot.result$plot.control$na.control)){
			plot.nas<-TRUE; nlines<-length(colours)+1
			}else{plot.nas<-FALSE; nlines<-length(colours)}
	}else{plot.nas<-FALSE; nlines<-length(colours)}

	# set y values
	y.vals<-seq(1, 0, length.out=nlines)
	breaks<-format(breaks, digits=2)	# should avoid rounding errors

	# draw
	plot(c(1)~c(1), ann=FALSE, axes=FALSE, type="n", xlim= xlim, ylim=c(-0.1, 1.1))
	for(i in 1: length(colours)){
		lines(x=c(0.5, 1), y=rep(y.vals[i], 2), col=colours[i], lwd= widths[i])
		text(x=0.5, y=y.vals[i], labels=paste(breaks[i], "-", breaks[i+1], sep=" "), pos=2, cex=cex)}
	
	# add NA line if applicable
	if(plot.nas){
		plot.list<-append(list(x=c(0.5, 1), y=rep(0, 2)), circleplot.result$plot.control$na.control)
		do.call(lines, plot.list)
		text(x=0.5, y=0, labels="NA", pos=2, cex=cex)
		}

	}	# end function

	# mtext("Key", side=3, adj=0, cex=cex, font=2) # again, can be drawn outside