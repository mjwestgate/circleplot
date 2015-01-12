# plot functions - called by user

# function to use the above code to draw a figure
circleplot<-function(
	input,	# a distance matrix (class 'dist') or square matrix (class matrix)
	cluster=TRUE, # should points be  rearranged using hclust? Defaults to true
	reduce=TRUE, # should points with no connections be removed?
	plot.control	# a matrix containing plot attributes. See ?circleplot
	)
	{
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
	if(plot.control$point.labels){		# extra x margins added only if those labels are to be drawn
	if(max(nchar(result$points$label))>2){x.lim<-colSums(rbind(x.lim, c(-0.3, 0.3)))}}

	# call plot code
	par(mar=rep(0.5, 4))	# set window attributes
	plot(x= result$points$x, y= result$points$y, 
		xlim=x.lim,
		type="n", ann=FALSE, axes=FALSE, asp=1)	# plot
	draw.curves(dataset, result, plot.control) 	# add lines
	do.call(points, as.list(result$points[, -1]))
	
	# label points
	if(plot.control$point.labels){
	if(max(nchar(result$points$label))<=2){
		text(result$points$x, result$points$y, label= result$points$label, col="white", cex=0.7)
	}else{
		label.position<-cut(result$points$x, breaks=c(min(result$points$x), 0, max(result$points$x)), 
			include.lowest=TRUE, labels=FALSE)
		text(result$points$x, result$points$y, label= result$points$label, 
			pos=c(2, 4)[label.position], col="black", cex=0.7)
		}
	} # end label plots

	invisible(list(locations=result, plot.control=plot.control))
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
			label= labels,
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
		text(x=0.5, y=y.vals[i], label=paste(breaks[i], "-", breaks[i+1], sep=" "), pos=2, cex=cex)}
	
	# add NA line if applicable
	if(plot.nas){
		plot.list<-append(list(x=c(0.5, 1), y=rep(0, 2)), circleplot.result$plot.control$na.control)
		do.call(lines, plot.list)
		text(x=0.5, y=0, label="NA", pos=2, cex=cex)
		}

	}	# end function

	# mtext("Key", side=3, adj=0, cex=cex, font=2) # again, can be drawn outside