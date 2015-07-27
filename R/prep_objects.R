# function to determine input type, and process accordingly
check.inputs<-function(
	input,
	reduce
	)
	{
	# set error messages
	if(any(c("dist", "matrix")==class(input))==F){
		stop('circleplot only accepts objects of class dist or matrix as inputs')}
	if(class(input)=="matrix"){
		if(dim(input)[1]!=dim(input)[2]){	
			stop('circleplot only accepts square matrices')}
		for(i in 1:dim(input)[1]){input[i, i]<-NA}	# set diagonal values to NA
		}

	# if there are no row or column headings, add these now
	if(class(input)=="dist"){
		if(length(attr(input, "Labels"))==0){
			attr(input, "Labels")<-c(1:attr(input, "Size"))}
	}else{	# i.e. class=="matrix"
		if(length(colnames(input))==0){
			rownames(input)<-c(1:dim(input)[1]); colnames(input)<-c(1:dim(input)[2])}}

	# work out if input is binary or continuous
	binary.test<-c(max(input, na.rm=TRUE)-min(input, na.rm=TRUE)==1,	
		max(input, na.rm=TRUE)==1)
	if(any(binary.test==FALSE)==FALSE){binary.test<-TRUE}else{binary.test<-FALSE}

	# binary matrices may contain rows/columns with no data; remove these before continuing
	if(binary.test & reduce){
		if(class(input)=="matrix"){dataset<-input}else{dataset<-as.matrix(input)}
		keep.rows<-as.numeric(which(apply(dataset, 1, FUN=function(x){sum(x, na.rm=TRUE)})>0))
		keep.cols<-as.numeric(which(apply(dataset, 2, FUN=function(x){sum(x, na.rm=TRUE)})>0))	
		keep.units<-sort(unique(c(keep.rows, keep.cols)))
		dataset<-dataset[keep.units, keep.units]
		if(class(input)=="dist"){dataset<-as.dist(dataset)}
	}else{dataset<-input}

	# check whether the input matrix is symmetric or asymmetric
	dist1<-as.vector(as.matrix(dataset))
	dist2<-as.vector(t(as.matrix(dataset)))
	asymmetry.vector<-apply(cbind(dist1, dist2), 1, FUN=function(x){
		na.test<-as.character(length(which(is.na(x))))
		switch(na.test,
			"0"={return(x[1]==x[2])},
			"1"={return(FALSE)},
			"2"={return(TRUE)})})	
	asymmetry.test<-any(asymmetry.vector==FALSE)

	# export these as a list-based S3 object that can be passed to later functions
	matrix.properties<-list(
		initial.class=class(input), 
		binary=binary.test,
		asymmetric= asymmetry.test,
		dist=dataset, #distance.matrix,
		source=input
		)

	return(matrix.properties)
	}	# end function



# function to compare supplied to default values, and return a combined data.frame with all columns
append.missed.columns<-function(
	input, 	# user-supplied values
	default	# default settings
	){
	if(class(default)=="data.frame"){
		specified.cols<-colnames(input)
		available.cols<-colnames(default)}
	if(class(default)=="list"){
		specified.cols<-names(input)
		available.cols<-names(default)}

	keep.cols<-sapply(available.cols, FUN=function(x){any(specified.cols==x)})
	add.cols<-which(keep.cols==FALSE)
	add.names<-names(add.cols)

	if(class(default)=="data.frame"){
		if(length(add.cols)>0){
			input<-as.data.frame(cbind(input, default[, add.cols]), stringsAsFactors=FALSE)
			new.cols<-c((ncol(input)-length(add.cols)+1):ncol(input))
			colnames(input)[new.cols]<-add.names
			}
		# ensure 'labels' column is placed first
		cols<-c(1:dim(input)[2])
		label.col<-which(colnames(input)=="labels")
		input<-input[, c(label.col, cols[-label.col])]
		}
	if(class(default)=="list"){
		if(length(add.cols)>0){
			input<-append(input, default[add.cols])
			new.entries<-c((length(input)-length(add.cols)+1):length(input))
			names(input)[new.entries]<-add.names
		}}
	# export
	return(input)
	}



# function to set plot defaults, and overwrite if new data is provided
set.plot.attributes<-function(
	input,	# result from check.inputs
	plot.control,
	cluster
	)
	{
	# FUNCTIONS ON PLOT CONTROL
	## GENERATE AND FILL AN EMPTY LIST FOR PLOT.CONTROL ##
	control.names<-c("plot.rotation", "par", "plot",
		"points", 
		"point.labels", 
		"line.breaks", "line.cols", "line.widths", # can these be relaced by a simple command, as in point.attr?
			# run test to see whether 'lines' is specified as a data.frame
			# if TRUE, then run line.attr() with appropriate defaults
			# if FALSE & line.cols etc. are specified, pass to line.attr()
		# i.e. you need your code to be consistent with earlier versions.
		# BUT with the aim that except for some circleplot-specific stuff (below), most values are simply passed to do.call
		"line.gradient", "line.expansion", "line.curvature", 
		"arrows",
		"na.control")
	plot.defaults<-vector("list", length=length(control.names))
	names(plot.defaults)<-control.names
	# turn off clustering for numeric matrices
	if(input$binary==FALSE)cluster<-FALSE

	# overwrite these values where others are provided
	if(missing(plot.control)==FALSE){
		# for backwards compatability, allow line.width (singular) instead of line.widths (plural) as input
		if(any(names(plot.control)=="line.width")){
			x<-which(names(plot.control)=="line.width")
			names(plot.control)[x]<-"line.widths"}
		# replace default plot.control info with any user-specified arguments
		names.provided<-names(plot.control)
		for(i in 1:length(plot.defaults)){
			if(any(names.provided==names(plot.defaults)[i])){
			entry.thisrun<-which(names.provided==names(plot.defaults)[i])
			plot.defaults[[i]]<-plot.control[[entry.thisrun]]
			}}}


	## FILL IN MISSING DATA WITH DEFAULTS ## 
	# 1. plot.rotation - has to be first for point.labels
	if(is.null(plot.defaults$plot.rotation)){plot.defaults$plot.rotation<-0}

	# 2. par
	par.default<-list(mar=rep(0.5, 4), cex=1)
	if(is.null(plot.defaults$par)){plot.defaults$par<-par.default}

	# 3. points
	distance.matrix<-input$dist
	if(is.matrix(input$dist)){
		n.points<-ncol(distance.matrix)
		label.vals<-colnames(distance.matrix)
	}else{ # i.e. dist
		n.points<-attr(distance.matrix, "Size")
		label.vals<-attr(distance.matrix, "Labels")}
	point.defaults<-data.frame(
			labels= label.vals,
			pch=19,
			col=rep(rgb(t(col2rgb("grey30")), maxColorValue=255), n.points),
			cex=1,
			stringsAsFactors=FALSE)
	rownames(point.defaults)<-point.defaults$labels
	# overwrite
	if(is.null(plot.defaults$points)){
		plot.defaults$points<-point.defaults
	}else{if(class(plot.defaults$points)=="data.frame"){
		plot.defaults$points<-append.missed.columns(plot.defaults$points, point.defaults)}
	}
	# ensure that any factors are converted to characters
	# note this assumes that only character strings (and not numeric values) will be interpreted as factors
	factor.test<-rep(FALSE, length(plot.defaults$points))
	for(i in 1:length(plot.defaults$points)){if(is.factor(plot.defaults$points[, i])){factor.test[i]<-TRUE}}
	if(any(factor.test)){
		cols<-which(factor.test==TRUE)
		plot.defaults$points[, cols]<-apply(plot.defaults$points[, cols], 2, function(x){as.character(x)})
		}
		
	# 4. point labels
	# set behaviour if no information has been given
	if(any(colnames(plot.defaults$point.labels)=="offset")==TRUE){
		label.distance<-mean(plot.defaults$point.labels$offset, na.rm=TRUE)+1
	}else{label.distance<-1.05}
	# create an object to allow proper positioning of labels
	edge.coords<-make.circle(n= n.points, alpha=plot.defaults$plot.rotation, k= label.distance)
	point.labels<-data.frame(
		labels= label.vals,
		x=edge.coords$x,
		y=edge.coords$y,
		cex=0.7,
		srt=edge.coords$theta*(180/pi),
		adj=0,
		col="black",
		stringsAsFactors=FALSE)
	point.labels$srt[which(point.labels$x<0)]<-point.labels$srt[which(point.labels$x<0)]+180
	point.labels$adj[which(point.labels$x<0)]<-1
	# if necessary, append to - or overwrite - supplied values
	if(class(plot.defaults$point.labels)=="data.frame"){	
		plot.defaults$point.labels<-append.missed.columns(plot.defaults$point.labels, point.labels)
		# ensure that any factors are converted to characters
		factor.test<-rep(FALSE, length(plot.defaults$point.labels))
		for(i in 1:length(plot.defaults$point.labels)){if(is.factor(plot.defaults$point.labels[, i])){factor.test[i]<-TRUE}}
		if(any(factor.test)){
			cols<-which(factor.test==TRUE)
			plot.defaults$point.labels[, cols]<-apply(plot.defaults$point.labels[, cols], 2, function(x){as.character(x)})}
		}
	if(is.logical(plot.defaults$point.labels)){
		if(plot.defaults$point.labels){plot.defaults$point.labels<-point.labels}}
	if(is.null(plot.defaults$point.labels)){
		plot.defaults$point.labels<-point.labels}

	# set defaults for line cuts, colours etc - set all to grey by default
	if(input$binary){
			cut.vals<-c(-1, 2)	; line.cols<-"grey30"
	}else{	# for numeric matrices
		overlap.zero<-min(distance.matrix, na.rm=TRUE)<0 & max(distance.matrix, na.rm=TRUE)>0
		if(overlap.zero){	# diverging colour palette
			cut.vals<-c(
				seq(min(distance.matrix, na.rm=TRUE)-0.001, 0, length.out=4)[1:3], 0, 
				seq(0, max(distance.matrix, na.rm=TRUE)+0.001, length.out=4)[2:4])
			line.cols<-brewer.pal(6, "RdBu")[6:1]
		}else{	# sequential colour palette
			if(any(distance.matrix=="Inf", na.rm=TRUE)){
				cut.vals<-as.numeric(c(
				seq(
					min(distance.matrix, na.rm=TRUE)-0.001,
					max(distance.matrix[-which(distance.matrix=="Inf")]+0.001, na.rm=TRUE), 
					length.out=6),
				"Inf"))
			}else{
				cut.vals<-seq(min(distance.matrix, na.rm=TRUE)-0.001, 
				max(distance.matrix, na.rm=TRUE)+0.001, length.out=7)}
			line.cols<-brewer.pal(6, "Purples")}
	}	# end colour selection	

	# arrow defaults
	arrow.defaults<-list(angle=10, length=0.07, distance=0.75)
	if(is.null(plot.defaults$arrows)){plot.defaults$arrows<-arrow.defaults
	}else{
		if(length(plot.control$arrows)!=3){
		plot.defaults$arrows<-append.missed.columns(plot.control$arrows, arrow.defaults)}}

	# add to plot.default
	if(is.null(plot.defaults$line.gradient)){plot.defaults$line.gradient<-FALSE}
	if(is.null(plot.defaults$line.breaks)){plot.defaults$line.breaks<-cut.vals}
	if(is.null(plot.defaults$line.cols)){plot.defaults$line.cols<-line.cols}
	if(is.null(plot.defaults$line.widths)){plot.defaults$line.widths<-rep(1, length(plot.defaults$line.cols))}
	if(is.null(plot.defaults$line.expansion)){plot.defaults$line.expansion<-0}
	if(is.null(plot.defaults$line.curvature)){
		if(input$asymmetric){
			plot.defaults$line.curvature <-c(add=0.2, multiply=0.3)
		}else{plot.defaults$line.curvature <-c(add=0.25, multiply=0.35)}}
	if(is.null(plot.defaults$na.control)){plot.defaults$na.control<-list(lwd=1, lty=2, col="grey")}


	## ERROR AVOIDANCE ##
	# ensure line.breaks incapsulate all values of input
	range.input<-range(input$dist, na.rm=TRUE)
	range.breaks<-range(plot.defaults$line.breaks)
	if(range.breaks[1]>range.input[1] | range.breaks[2]<range.input[2]){
		stop(paste(
			'Specified line.breaks do not encapsulate the range of values of the input matrix (min=',
			round(range.input[1], 2), ', max=', round(range.input[2], 2), 
			'); please specify new breaks', sep=""))
		}

	# ensure correct number of colours and widths added
	if(length(plot.defaults$line.cols)!=(length(plot.defaults$line.breaks)-1)){
		stop("Specified number of line.cols does not match number of line.breaks; please adjust inputs to plot.control")}

	# correct line.width if necessary
	# only change if defaults have been overwritten with poor inputs
	if(length(plot.defaults$line.widths)!=length(plot.defaults$line.cols)){	
		if(length(plot.defaults$line.widths)==1){
			plot.defaults$line.widths<-rep(plot.defaults$line.widths, length(plot.defaults$line.cols))
		}else{if(length(plot.defaults$line.widths)>1){	# i.e. if a min and max is given, choose only the max value
			plot.defaults$line.widths<-rep(max(plot.defaults$line.widths, na.rm=TRUE), length(line.cols))
		}}}

	# Invert direction of line.expansion
	plot.defaults$line.expansion<-1-plot.defaults$line.expansion
	# This is a bit pointless, but is included for consistency with earlier versions.

	# if line.curvature is a list, convert to vector	
	if(is.list(plot.defaults$line.curvature)){
		plot.defaults$line.curvature<-unlist(plot.defaults$line.curvature)}

	# if point.attr are <v0.3 standard, convert to new format (i.e. for passing to do.call(points, ...)
	if(any(colnames(plot.defaults$points)=="colour")){
		x<-which(colnames(plot.defaults$points)=="colour")
		colnames(plot.defaults$points)[x]<-"col"}
	if(any(colnames(plot.defaults$points)=="size")){
		x<-which(colnames(plot.defaults$points)=="size")
		colnames(plot.defaults$points)[x]<-"cex"}
	if(any(colnames(plot.defaults$points)=="pch")==FALSE){
		plot.defaults$points$pch<-rep(19, dim(plot.defaults$points)[1])}
	# ensure that labels are characters, not factors
	plot.defaults$points$labels<-as.character(plot.defaults$points$labels)


	# FUNCTIONS ON INPUT: SET POINT AND LINE ATTRIBUTES
	# this behaviour partially depends on whether labels should be added
	label.suppress.test<-is.logical(plot.defaults$point.labels) & length(plot.defaults$point.labels)==1
	# make points for plotting
	circle.points<-as.data.frame(
		make.circle(n.points, alpha=plot.defaults$plot.rotation)[, 2:3])
	# reorder (or not)
	if(cluster){
		if(input$binary){dist.data<-input$dist}else{dist.data<-as.dist(1-(sqrt(input$dist^2)))}
		cluster.result<-hclust(dist.data)
		circle.points$labels<-label.vals[cluster.result$order]
		if(label.suppress.test==FALSE){plot.defaults$point.labels$labels<-label.vals[cluster.result$order]}
	#	if(input$binary){
		# dist.data<-input$dist
		# cluster.result<-hclust(dist.data)
		# node.labels<-label.vals[cluster.result$order]
	#	}else{ # add function for calculating numeric distance
		#	circle.points$labels<-label.vals # temporary fix
			#cluster.result<-cluster.numeric(locations=circle.points, input=input$dist)
			#node.labels<-cluster.result$labels}
		# circle.points$labels<-node.labels
		# if(label.suppress.test==FALSE){plot.defaults$point.labels$labels<-node.labels}
	}else{circle.points$labels<-label.vals}
	# add supp. info
	circle.points$labels<-as.character(circle.points$labels)
	circle.points<-circle.points[order(circle.points$labels), ]
	circle.points<-merge(circle.points, plot.defaults$points, by="labels")
	rownames(circle.points)<-circle.points$labels

	# then line values
	# begin by collapsing asymmetric matrices into two lower-triangular matrices; one for max value, the other for direction
	# the raw values are retained for symmetric matrices
	direction.matrix<-as.dist(input$dist)
	direction.matrix[1:length(direction.matrix)]<-3 # three is the category given below for no arrows drawn

	# make a simple 'direction' matrix
	if(input$asymmetric){
		# calculate a single value, that is the max (absolute) value of both inputs
		comparison.dframe<-cbind(as.vector(as.dist(input$source)), as.vector(as.dist(t(input$source))))
		sign.dframe<-sign(comparison.dframe)
		if(any(sign.dframe==0, na.rm=TRUE)){sign.dframe[which(sign.dframe==0)]<-1}
		positive.dframe<-sqrt(comparison.dframe^2)
		value.initial<-apply(positive.dframe, 1, FUN=function(x){
			if(any(is.na(x)==FALSE)==FALSE){return(NA)}else{return(max(x, na.rm=TRUE))}})
		if(input$binary){
			value.final<-value.initial
			first.value<-comparison.dframe[, 1]==1
			same.value<-apply(comparison.dframe, 1, sum)==2
		}else{ # i.e. for numeric matrices
			sign.lookup<-apply(positive.dframe, 1, FUN=function(x){
				if(any(is.na(x)==FALSE)==FALSE){return(NA)
				}else{return(which(x==max(x, na.rm=TRUE))[1])}})
			sign.value<-apply(cbind(sign.dframe, sign.lookup), 1, FUN=function(x){
				if(is.na(x[3])){return(NA)}else{return(x[x[3]])}})
			value.final<-value.initial*sign.value
			# calculate direction, using categories given by plot.defaults$line.breaks
			# to specify whether an arrow should be drawn.
			cut.matrix<-matrix(data=cut(comparison.dframe, 
				breaks=plot.defaults$line.breaks, include.lowest=TRUE, labels=FALSE),
				nrow=nrow(comparison.dframe), ncol=2)
			same.value<-apply(cut.matrix, 1, FUN=function(x){
				if(any(is.na(x))){
					test<-which(is.na(x))
					if(length(test)==1){return(FALSE)}else{return(TRUE)}
				}else{return(x[1]==x[2])}})
			first.value<-sign.lookup==1
			if(any(is.na(first.value))){first.value[which(is.na(first.value))]<-TRUE}
		}
		# export line values
		input$dist<-as.dist(input$dist)
		input$dist[1:length(input$dist)]<-value.final
		# export directions
		direction.categories<-unlist(apply(cbind(first.value, same.value), 1, FUN=function(x){
			if(x[2]){return(3)}else{
				if(x[1]){return(1)}else{return(2)}}
			}))
		direction.matrix[1:length(direction.matrix)]<-direction.categories
	}

	# now work out line attr
	line.list<-data.frame(t(combn(label.vals, 2)), stringsAsFactors=FALSE)
		colnames(line.list)<-c("sp1", "sp2")
	line.list$value<-as.numeric(input$dist)
	line.list$direction<-as.numeric(direction.matrix)
	if(input$binary){
		# remove 'absent' connections
		line.list<-line.list[-which(line.list$value==0), ]
	}else{
		# order line list by effect size
		effect.size<-line.list$value^2
		line.list<-line.list[order(effect.size), ]}
	# place NA values first (so they are underneath drawn values)
	line.list<-line.list[c(
		which(is.na(line.list$value)==TRUE), 
		which(is.na(line.list$value)==FALSE)), ]

	# ALLOW USER SPECIFICATION OF X, Y LIMITS
	# determine margins
	x.lim<-c(min(circle.points$x), max(circle.points$x))
	# extra x margins added
	# note this works beacuse set.plot.attributes allows FALSE as the only logical operator to point.labels
	if(label.suppress.test==FALSE){
		max.label<-max(nchar(circle.points$labels))
		x.expansion<-max.label*0.03
		x.lim<-colSums(rbind(x.lim, c(-x.expansion, x.expansion)))
	}else{x.lim<-c(-1, 1)}

	# set plot attributes
	plot.list<-list(x=circle.points$x, y=circle.points$y, 
		xlim=x.lim, ylim=x.lim, type="n", ann=FALSE, axes=FALSE, asp=1)
	if(is.null(plot.defaults$plot)){
		plot.defaults$plot<-plot.list
	}else{
		user.data<-plot.defaults$plot
		attr.list<-names(user.data)
		available.attr<-names(plot.list)
		keep.cols<-sapply(available.attr, FUN=function(x){any(attr.list==x)})
		add.cols<-which(keep.cols==FALSE)
		if(length(add.cols)>0){plot.defaults$plot<-append(user.data, plot.list[as.numeric(add.cols)])
		}else{plot.defaults$plot<-user.data}
	}

	# return all outputs
	result<-list(
		binary= input$binary,
		asymmetric=input$asymmetric,
		points=circle.points, 
		lines=line.list, 
		plot.control=plot.defaults)
	return(result)
	}


# function to add get a data.frame in the correct format to draw a key from a circleplot object
get.key.dframe<-function(circleplot.result, exclude.lines, reverse, cex){

	# get info from source object
    breaks <- circleplot.result$plot.control$line.breaks
		breaks <- format(breaks, digits = 2)
    colours <- circleplot.result$plot.control$line.cols
    widths <- circleplot.result$plot.control$line.widths

	# sort out col names with and without NA values
	na.info<-circleplot.result$plot.control$na.control
	na.present<-is.list(circleplot.result$plot.control$na.control) & any(is.na(circleplot.result$lines$value))
	col.names<-c("col", "lty", "lwd")
	if(na.present){col.names<-sort(unique(c(col.names, names(na.info))))}
	nlines<-length(colours)+1

	# make line data.frame
	line.data<-as.data.frame(matrix(data=NA, nrow=nlines, ncol=length(col.names)))
		colnames(line.data)<-col.names
	line.data$col[1:length(colours)]<-colours
	line.data$lty[1:length(colours)]<-1
	line.data$lwd[1:length(widths)]<-widths

	# add NA information
	if(na.present){for(i in 1:length(na.info)){
		col<-which(colnames(line.data)==names(na.info)[i])
		line.data[nrow(line.data), col]<-na.info[i]}}
	
	# group data into a single data.frame that can be passed to lines by do.call
	line.frame<-cbind(data.frame(x0=0.5, x1=1, y0=1, y1=1),line.data)
	text.frame<-data.frame(x=rep(0.5, nlines), y=1, cex=cex, pos=2, stringsAsFactors=FALSE)
	labels.initial<-c(paste(breaks[c(1:(length(breaks)-1))], "-", breaks[c(2:length(breaks))], sep=" "), "NA")
	text.frame$labels<-labels.initial

	# exclude lines as requested by user
	exclude.na<-c(na.present==FALSE | any(exclude.lines==nrow(line.data)))
	if(exclude.na){
		line.frame<-line.frame[-nlines, ]
		text.frame<-text.frame[-nlines, ]
		}
	if(any(exclude.lines==nlines)){exclude.lines<-exclude.lines[-which(exclude.lines==nlines)]}
	exclude.test<-c(length(exclude.lines)>0 & any(exclude.lines==999)==FALSE)
	if(exclude.test){	
		line.frame<-line.frame[-exclude.lines, ]
		text.frame<-text.frame[-exclude.lines, ]
		}
	nlines<-nrow(line.frame)

	# sort out y values (in reverse order if requested)
	if(reverse){
		if(exclude.na){line.order<-c(nlines:1)}else{line.order<-c((nlines-1):1, nlines)} # NA values always placed last
		line.frame<-line.frame[line.order, ]
		text.frame<-text.frame[line.order, ]}
	y.vals <- seq(1, 0, length.out = nlines)
	line.frame$y0<-y.vals; line.frame$y1<-y.vals
	text.frame$y<-y.vals

	# return object
	return(list(lines=line.frame, text=text.frame))
	}
