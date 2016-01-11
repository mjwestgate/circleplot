# function to determine input type, and process accordingly
check.inputs<-function(
	input,
	reduce
	)
	{
	# set error messages
	# incorrect input class
	if(any(c("dist", "matrix", "data.frame")==class(input))==F){
		stop('circleplot only accepts inputs from the following classes: dist, matrix or data.frame')}
	
	# ensure that long and wide versions are returned, regardless of input
	switch(class(input),
		"matrix"={
			if(dim(input)[1]!=dim(input)[2]){stop('circleplot only accepts square matrices')}
			wide<-input; for(i in 1:dim(wide)[1]){wide[i, i]<-NA}	# set diagonal values to NA
			# if there are no row or column headings, add these now
			long<-make.long.format(input)
			if(length(colnames(wide))==0){wide<-make.wide.format(long)}
			check.distance<-make.dist.format(long)
			distance<-check.distance$dist.matrix
			asymmetric<-check.distance$asymmetric
			},
		"data.frame"={
			if(ncol(input)<3){stop('input data.frame has too few columns; please supply data with n>=3 columns')}#}
			long<-input
			wide<-make.wide.format(input[, 1:3])
			check.distance<-make.dist.format(input)
			distance<-check.distance$dist.matrix
			asymmetric<-check.distance$asymmetric
			},
		"dist"={
			if(length(attr(input, "Labels"))<1){
				attr(input, "Labels")<-paste("V", c(1:attr(input, "Size")), sep="")}
			wide<-as.matrix(input)
			long<-make.long.format(wide)
			check.distance<-make.dist.format(long) # this is needed to avoid NA values in the distance matrix
			distance<-check.distance$dist.matrix
			asymmetric<-check.distance$asymmetric
			})

	# work out if input is binary or continuous
	in.vals<-long[is.na(long[, 3])==FALSE, 3]
	n.vals<-length(unique(in.vals))
	if(n.vals==1){if(unique(in.vals)==1){binary.test<-TRUE}else{binary.test<-FALSE}}
	if(n.vals==2){if(max(in.vals)==1 & min(in.vals)==0){binary.test<-TRUE
		}else{binary.test<-FALSE}}
	if(n.vals>2){binary.test<-FALSE}

	# binary matrices may contain rows/columns with no data; remove these before continuing
	if(reduce){
		keep.rows<-apply(wide, 1, function(x){length(which(is.na(x)))!=length(x)})
		keep.cols<-apply(wide, 2, function(x){length(which(is.na(x)))!=length(x)})
		# both are needed for asymmetric matrices
		keep.both<-apply(cbind(keep.rows, keep.cols), 1, function(x){any(x==TRUE)})
		keep.units<-as.numeric(which(keep.both))
		wide<-wide[keep.units, keep.units]
		distance<-as.dist(as.matrix(distance)[keep.units, keep.units])
		# ensure long format matches
		keep.text<-colnames(wide)[keep.units]
		keep.test<-lapply(long[, 1:2], function(x, comp){
			sapply(x, function(y, text){any(text==y)}, text=comp)}, comp=keep.text)
		keep.vector<-apply(as.data.frame(keep.test), 1, function(x){all(x)})
		long<-long[which(keep.vector), ]	
		}

	# export these as a list-based S3 object that can be passed to later functions
	matrix.properties<-list(
		binary=binary.test,
		asymmetric= asymmetric,
		wide=wide, # point generation/manipulation requires matrices
		long=long, 	# line attr requires a data.frame
		distance=distance	# clustering requires a distance matrix
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
			input<-merge(input, default[, c(1, add.cols)], by="labels", all.x=FALSE, all.y=TRUE)
			}
		# ensure 'labels' column is placed first
		# cols<-c(1:dim(input)[2])
		# label.col<-which(colnames(input)=="labels")
		# input<-input[, c(label.col, cols[-label.col])]
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
	plot.control
	)
	{
	# FUNCTIONS ON PLOT CONTROL
	## GENERATE AND FILL AN EMPTY LIST FOR PLOT.CONTROL ##
	control.names<-c("plot.rotation", "par", "plot",
		"points", 
		"point.labels", 
		"line.breaks", "line.cols", "line.widths", 
		"line.gradient", "line.expansion", "line.curvature", 
		"arrows",
		"na.control")
	plot.defaults<-vector("list", length=length(control.names))
	names(plot.defaults)<-control.names
	# turn off clustering for numeric matrices
	# if(input$binary==FALSE)cluster<-FALSE

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
	# 1. plot.rotation
	if(is.null(plot.defaults$plot.rotation)){plot.defaults$plot.rotation<-0}

	# 2. par
	par.default<-list(mar=rep(0.5, 4), cex=1)
	if(is.null(plot.defaults$par)){plot.defaults$par<-par.default}

	# 3. points
	n.points<-ncol(input$wide)
	label.vals<-colnames(input$wide)
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
	point.labels<-data.frame(
		labels= label.vals,
		cex=0.7,
		adj=0,
		col="black",
		stringsAsFactors=FALSE)

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

	# arrow defaults
	arrow.defaults<-list(angle=10, length=0.07, distance=0.75)
	if(is.null(plot.defaults$arrows)){plot.defaults$arrows<-arrow.defaults
	}else{
		if(length(plot.control$arrows)!=3){
		plot.defaults$arrows<-append.missed.columns(plot.control$arrows, arrow.defaults)}}

	# set line attributes
	line.attr<-lapply(plot.defaults, function(x){is.null(x)==FALSE})[6:8]
	if(any(line.attr==TRUE)){
		if(line.attr$line.breaks){n.lines<-length(plot.defaults$line.breaks)-1}
		if(line.attr$line.cols){n.lines<-length(plot.defaults$line.cols)}
		if(line.attr$line.widths){n.lines<-length(plot.defaults$line.widths)}
	}else{n.lines=5}

	# ensure brewer.pal returns values for nlines <3
	brewer.pal.safe<-function(n, pal, type){
		if(n>=3){brewer.pal(n, pal)
		}else{
			if(type=="increasing"){brewer.pal(3, pal)[sort(c(3:1)[c(1:n)])]}
			if(type=="diverging"){brewer.pal(3, pal)[c(1, 3)]} # assumes that no-one will be able to call diverging w n=1
		}}

	# set defaults for line cuts, colours etc - set all to grey by default
	if(input$binary){
			cut.vals<-c(-1, 2)	; line.cols<-"grey30"
	}else{	# for numeric matrices
		overlap.zero<-min(input$long[, 3], na.rm=TRUE)<0 & max(input$long[, 3], na.rm=TRUE)>0
		if(overlap.zero){	# diverging colour palette
			max.val<-max(sqrt(input$long[, 3]^2), na.rm=TRUE)+0.001
			cut.vals<-seq(-max.val, max.val, length.out=n.lines+1)
			line.cols<-brewer.pal.safe(n.lines, "RdBu", type="diverging")[c(n.lines:1)]
		}else{	# sequential colour palette
			if(any((input$long[, 3]^2)==Inf, na.rm=TRUE)){
				cut.vals<-seq(
					min(input$long[which(input$long[, 3]!=-Inf), 3], na.rm=TRUE)-0.001,
					max(input$long[which(input$long[, 3]!=Inf), 3]+0.001, na.rm=TRUE), 
					length.out=n.lines+1)
				if(any(input$long[, 3]==Inf, na.rm=TRUE)){cut.vals[length(cut.vals)]<-Inf}
				if(any(input$long[, 3]==-Inf, na.rm=TRUE)){cut.vals[1]<-(-Inf)}				
			}else{
				cut.vals<-seq(min(input$long[, 3], na.rm=TRUE)-0.001, 
				max(input$long[, 3], na.rm=TRUE)+0.001, length.out=n.lines+1)}
			line.cols<-brewer.pal.safe(n.lines, "Purples", type="increasing")}
	}	# end colour selection	

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
	range.input<-range(input$wide, na.rm=TRUE)
	range.breaks<-range(plot.defaults$line.breaks)
	if(range.breaks[1]>range.input[1] | range.breaks[2]<range.input[2]){
		stop(paste(
			'Specified line.breaks do not encapsulate the range of values of the input matrix (min=',
			round(range.input[1], 2), ', max=', round(range.input[2], 2), 
			'); please specify new breaks', sep=""))
		}

	# ensure correct number of colours and widths added
	if(length(plot.defaults$line.cols)!=(length(plot.defaults$line.breaks)-1)){
		if(length(plot.defaults$line.cols!=1)){
			stop("Specified number of line.cols does not match number of line.breaks; please adjust inputs to plot.control")}}

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

	return(plot.defaults)
	}


# function to take results from set.plot.attributes, and use them to create data.frames full of relevant information for plotting
calc.circleplot<-function(x, plot.options, cluster){

	# POINTS & LABELS
	n.points<-nrow(plot.options$points)
	circle.points<-as.data.frame(make.circle(n.points, alpha= plot.options$plot.rotation)[, 2:3])

	if(any(colnames(plot.options$point.labels)=="offset")==TRUE){
		label.distance<-mean(plot.options$point.labels$offset, na.rm=TRUE)+1
	}else{label.distance<-1.05}
	circle.labels<-as.data.frame(make.circle(n= n.points, alpha= plot.options$plot.rotation, k= label.distance)[, c(2, 3, 1)])
	circle.labels$srt<-circle.labels$theta*(180/pi)

	# clustering
	if(cluster){
		cluster.result<-as.data.frame(hclust(x$distance)[3:4])
		new.order<-order(cluster.result$order)
	}else{new.order<-c(1:nrow(circle.points))}

	# get information on points and labels, but do not add x,y coordinates
	point.dframe<-plot.options$points
	point.dframe$order.auto<-new.order
	label.dframe<-plot.options$point.labels

	# reorder as necessary
	if(any(grepl("order", colnames(point.dframe)))){
		order.cols<-which(grepl("order", colnames(point.dframe)))
		if(length(order.cols)>1){
			row.order<-do.call(order, as.list(point.dframe[, order.cols]))
		}else{
			row.order<-order(point.dframe[, order.cols])}
		point.dframe<-point.dframe[row.order, -order.cols]
		label.dframe<-label.dframe[row.order, ]
	}

	# add coordinates
	point.dframe<-cbind(circle.points, point.dframe)
	label.dframe<-cbind(circle.labels[, c(1, 2, 4)], label.dframe)
	point.list<-list(point.dframe, label.dframe)
	names(point.list)<-c("points", "labels")

	# correct label presentation
	point.list$labels$srt[which(point.list$labels$x<0)]<-point.list$labels$srt[which(point.list$labels$x<0)]+180
	point.list$labels$adj<-0
	point.list$labels$adj[which(point.list$labels$x<0)]<-1

	# LINES
	line.list<-x$long
	colnames(line.list)[3]<-"value"
	# work out number and order of lines to be retained/plotted
	if(x$binary){
		# remove 'absent' connections
		line.list<-line.list[which(line.list$value==1), ]
		if(nrow(line.list)==0){stop("No connections are available to draw - binary input must contain some present values")}
	}else{
		# order line list by effect size
		effect.size<-line.list$value^2
		line.list<-line.list[order(effect.size), ]}
	# place NA values first (so they are underneath drawn values)
	if(any(is.na(line.list$value))){
		line.list<-line.list[c(
			which(is.na(line.list$value)==TRUE), 
			which(is.na(line.list$value)==FALSE)), ]}

	# set line colours & widths. 
	line.cuts<-cut(line.list$value, plot.options$line.breaks, 
		include.lowest=TRUE, right=TRUE, labels=FALSE)
	line.defaults<-data.frame(
		col= plot.options$line.cols[line.cuts],
		lwd= plot.options$line.widths[line.cuts],
		lty=1,
		arrows=TRUE,
		stringsAsFactors=FALSE)

	# whether or not arrows should be added
	if(class(plot.options$na.control)=="list"){	
		if(any(is.na(line.list$value))){
			line.defaults$col[which(is.na(line.list$value))]<-plot.options$na.control$col
			line.defaults$lwd[which(is.na(line.list$value))]<-plot.options$na.control$lwd
			line.defaults$lty[which(is.na(line.list$value))]<-plot.options$na.control$lty
			line.defaults$arrows[which(is.na(line.list$value))]<-FALSE}
	}else{
		if(any(is.na(line.list$value))){
			keep.rows<-which(is.na(line.list$value)==FALSE)
			line.list<-line.list[keep.rows, ]
			line.defaults<-line.defaults[keep.rows, ]}
	}

	# add infomation from line.defaults to line.list
	keep.cols<-sapply(colnames(line.defaults), FUN=function(x){any(colnames(line.list)==x)})
	add.cols<-which(keep.cols==FALSE)
	add.names<-names(add.cols)
	line.list<-cbind(line.list, line.defaults[, add.cols])
	new.entries<-c((ncol(line.list)-length(add.cols)+1):ncol(line.list))
	colnames(line.list)[new.entries]<-add.names


	# PLOT
	x.lim<-c(min(point.dframe$x), max(point.dframe$x))
	# extra x margins added
	# note this works beacuse set.plot.attributes allows FALSE as the only logical operator to point.labels
	label.suppress.test<-is.logical(plot.options$point.labels) & length(plot.options$point.labels)==1
	if(label.suppress.test==FALSE){
		max.label<-max(nchar(point.dframe$labels))
		x.expansion<-max.label*0.03
		x.lim<-colSums(rbind(x.lim, c(-x.expansion, x.expansion)))
	}else{x.lim<-c(-1, 1)}
	# set plot attributes
	plot.list<-list(x= point.dframe$x, y= point.dframe$y, 
		xlim=x.lim, ylim=x.lim, type="n", ann=FALSE, axes=FALSE, asp=1)
	if(is.null(plot.options$plot)==FALSE){
		user.data<-plot.options$plot
		attr.list<-names(user.data)
		available.attr<-names(plot.list)
		keep.cols<-sapply(available.attr, FUN=function(x){any(attr.list==x)})
		add.cols<-which(keep.cols==FALSE)
		if(length(add.cols)>0){plot.list<-append(user.data, plot.list[as.numeric(add.cols)])
		}}

	# return all outputs
	result.list<-append(list(par=plot.options$par, plot=plot.list), point.list)
	result.list<-append(result.list, list(lines=line.list))
	attr(result.list, "binary")<-x$binary
	attr(result.list, "asymmetric")<-x$asymmetric
	return(result.list)
	}




# function to add get a data.frame in the correct format to draw a key from a circleplot object
get.key.dframe<-function(circleplot.result, exclude.lines, reverse, cex, right){

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
	line.frame<-cbind(data.frame(x0=0, x1=1, y0=1, y1=1), line.data)
	text.labels<-c(
		paste(breaks[c(1:(length(breaks)-1))], "-", breaks[c(2:length(breaks))], sep=" "), "NA")

	# exclude lines as requested by user
	exclude.na<-c(na.present==FALSE | any(exclude.lines==nrow(line.data)))
	if(exclude.na){
		line.frame<-line.frame[-nlines, ]
		text.labels<-text.labels[-nlines]
		}
	if(any(exclude.lines==nlines)){exclude.lines<-exclude.lines[-which(exclude.lines==nlines)]}
	exclude.test<-c(length(exclude.lines)>0 & any(exclude.lines==999)==FALSE)
	if(exclude.test){	
		line.frame<-line.frame[-exclude.lines, ]
		text.labels<-text.labels[-exclude.lines]
		}
	nlines<-nrow(line.frame)

	# sort out y values (in reverse order if requested)
	if(reverse){
		if(exclude.na){line.order<-c(nlines:1)}else{line.order<-c((nlines-1):1, nlines)} # NA values always placed last
		line.frame<-line.frame[line.order, ]
		text.labels <-text.labels[line.order]}
	y.vals <- seq(1, 0, length.out = nlines)
	line.frame$y0<-y.vals; line.frame$y1<-y.vals
	text.list<-list(at=y.vals, labels=text.labels, tick=FALSE, las=1)

	# return object
	return(list(lines=line.frame, text=text.list))
	}


# function to work out how many panels to include for run.circleplot.multiple
panel.dims<-function(n){
	x<-sqrt(n)
	low<-floor(x)
	high<-ceiling(x)
	if((low*high)>=n){return(c(low, high))
	}else{return(rep(high, 2))}
	}

