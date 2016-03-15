# function to determine input type, and process accordingly
check.inputs<-function(
	input,
	reduce
	)
	{
	# set error messages
	# incorrect input class
	if(any(c("dist", "matrix", "data.frame", "list")==class(input))==FALSE){
		stop('circleplot only accepts inputs from the following classes: dist, matrix data.frame, or a list of the same')}
	
	# ensure that long and wide versions are returned, regardless of input
	switch(class(input),
		"matrix"={
			if(dim(input)[1]!=dim(input)[2]){stop('circleplot only accepts square matrices')}
			wide<-input; for(i in 1:dim(wide)[1]){wide[i, i]<-NA}	# set diagonal values to NA
			# if there are no row or column headings, add these now
			long<-list(make.long.format(input))
			if(length(colnames(wide))==0){wide<-lapply(long, make.wide.format)
			}else{wide<-list(wide)}
			check.distance<-make.dist.format(long[[1]])
			distance<-check.distance$dist.matrix
			asymmetric<-check.distance$asymmetric
			},
		"data.frame"={
			if(ncol(input)<3){stop('input data.frame has too few columns; please supply data with n>=3 columns')}#}
			long<-list(input)
			wide<-list(make.wide.format(input[, 1:3]))
			check.distance<-make.dist.format(input)
			distance<-check.distance$dist.matrix
			asymmetric<-check.distance$asymmetric
			},
		"dist"={
			if(length(attr(input, "Labels"))<1){
				attr(input, "Labels")<-paste("V", c(1:attr(input, "Size")), sep="")}
			wide<-list(as.matrix(input))
			long<-lapply(wide, make.long.format)
			check.distance<-make.dist.format(long[[1]]) # this is needed to avoid NA values in the distance matrix
			distance<-check.distance$dist.matrix
			asymmetric<-check.distance$asymmetric
			},
		"list"={
			result<-clean.list(input, reduce)
			long<-result$long
			wide<-result$wide
			distance<-result$distance
			asymmetric<-result$asymmetric
		})

	# work out if input is binary or continuous
	binary.test<-all(unlist(lapply(long, binary.test.fun)))

	# binary matrices may contain rows/columns with no data; remove these before continuing
	if(class(input)!="list" & reduce){
		keep.rows<-apply(wide[[1]], 1, function(x){length(which(is.na(x)))!=length(x)})
		keep.cols<-apply(wide[[1]], 2, function(x){length(which(is.na(x)))!=length(x)})
		# both are needed for asymmetric matrices
		keep.both<-apply(cbind(keep.rows, keep.cols), 1, function(x){any(x==TRUE)})
		keep.units<-as.numeric(which(keep.both))
		wide<-list(wide[[1]][keep.units, keep.units])
		distance<-as.dist(as.matrix(distance)[keep.units, keep.units])
		# ensure long format matches
		keep.text<-colnames(wide[[1]])[keep.units]
		keep.test<-lapply(long[[1]][, 1:2], function(x, comp){
			sapply(x, function(y, text){any(text==y)}, text=comp)}, comp=keep.text)
		keep.vector<-apply(as.data.frame(keep.test), 1, function(x){all(x)})
		long<-list(long[[1]][which(keep.vector), ])
		}

	# ensure distance matrices do not contain infinite values
	for(i in 1:2){distance<-remove.inf.values(c(Inf, -Inf)[i], distance)}
	# note that this doesn't affect plotting because all non-clustering code uses wide or long format

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


# Function to ensure distance matrices do not contain infinite values; called exclusively by check.inputs
remove.inf.values<-function(x, distmat){
	if(any(distmat ==x)){
		overlap.zero<-min(distmat)<0 & max(distmat)>0
		vals<-distmat[which(distmat!=x)]
		max.val<-max(sqrt(vals^2))*2
		distmat[which(distmat==x)]<-(max.val * sign(x))}
	return(distmat)
	}


# Function to determine whether an input is binary or continuous; called exclusively by check.inputs
binary.test.fun<-function(x){
	in.vals<-x[is.na(x[, 3])==FALSE, 3]
	n.vals<-length(unique(in.vals))
	if(n.vals==1){if(unique(in.vals)==1){binary.test<-TRUE}else{binary.test<-FALSE}}
	if(n.vals==2){if(max(in.vals)==1 & min(in.vals)==0){binary.test<-TRUE
		}else{binary.test<-FALSE}}
	if(n.vals>2){binary.test<-FALSE}
	return(binary.test)
	}


# function to compare supplied to default values, and return a combined data.frame with all columns
append.missed.columns<-function(
	input, 	# user-supplied values
	default 	# default settings
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
	reduce, # should this be here? Or put elsewhere, perhaps with cluster?
	style="classic"
	)
	{
	## GENERATE AND FILL AN EMPTY LIST FOR PLOT.CONTROL ##
	control.names<-c("style", "plot.rotation", "par", "plot",
		"points", 
		"point.labels", 
		"line.breaks", "line.cols", "line.widths", 
		"line.gradient", "line.expansion", "line.curvature", 
		"arrows",
		"border",
		"na.control")
	plot.defaults<-vector("list", length=length(control.names))
	names(plot.defaults)<-control.names

	# overwrite these values where others are provided
	if(missing(plot.control)==FALSE){
		# for backwards compatability, allow line.width (singular) instead of line.widths (plural) as input
		if(any(names(plot.control)=="line.width")){
			x<-which(names(plot.control)=="line.width")
			names(plot.control)[x]<-"line.widths"}
		# replace default plot.control info with any user-specified arguments
		for(i in 1:length(plot.defaults)){
			if(any(names(plot.control)==names(plot.defaults)[i])){
			entry.thisrun<-which(names(plot.control)==names(plot.defaults)[i])
			plot.defaults[i]<-plot.control[entry.thisrun]
			}}
		}

	## FILL IN MISSING DATA WITH DEFAULTS ## 
	plot.defaults$style<-style

	# 1. plot.rotation
	if(is.null(plot.defaults$plot.rotation)){plot.defaults$plot.rotation<-0}

	# 2. par
	par.default<-list(mar=rep(0.5, 4), cex=1)
	if(is.null(plot.defaults$par)){plot.defaults$par<-par.default}

	# 3. points
	matrix.labels<-lapply(input$wide, colnames)[[1]]
	if(is.null(plot.defaults$points)){ #  | reduce
		n.points<-lapply(input$wide, ncol)[[1]]  #ncol(input$wide)
		label.vals<-matrix.labels
	}else{
		n.points<-nrow(plot.defaults$points)
		label.vals<-plot.defaults$points$labels
		# check these match
		if(all(sort(matrix.labels)==sort(label.vals))==FALSE){
			stop("supplied point labels do not match those from the input matrix or data.frame")}
		}
	# generate a 'null' data.frame
	point.defaults<-data.frame(
			labels= label.vals,
			pch=19,
			col=rep(rgb(t(col2rgb("grey30")), maxColorValue=255), n.points),
			cex=1,
			stringsAsFactors=FALSE)
	# rownames(point.defaults)<-point.defaults$labels 
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
	# if(any(colnames(plot.defaults$points)=="order")){plot.defaults$point.labels$order<-plot.defaults$points$order}

	# arrow defaults
	arrow.defaults<-list(angle=10, length=0.07, distance=0.75)
	if(is.null(plot.defaults$arrows)){plot.defaults$arrows<-arrow.defaults
	}else{
		if(length(plot.control$arrows)!=3){
		plot.defaults$arrows<-append.missed.columns(plot.control$arrows, arrow.defaults)}}

	# set line attributes
	line.locs<-c(
		which(names(plot.defaults)=="line.gradient"):
		which(names(plot.defaults)=="line.curvature"))
	line.attr<-lapply(plot.defaults, function(x){is.null(x)==FALSE})[line.locs]
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
	line.vals<-unlist(lapply(input$long, function(x){x[, 3]}))
	line.vals<-line.vals[which(is.na(line.vals)==FALSE)]
	if(input$binary){
			cut.vals<-c(-1, 2)	; line.cols<-"grey30"
	}else{	# for numeric matrices
		line.vals.short<-line.vals
		line.vals.short<-line.vals.short[which(line.vals.short!=Inf)]
		line.vals.short<-line.vals.short[which(line.vals.short!=-Inf)]
		if(length(line.vals.short)==0)stop("circleplot cannot draw matrices that consist entirely of infinite values")
		overlap.zero<-min(line.vals)<0 & max(line.vals)>0
		if(overlap.zero){	# diverging colour palette
			max.val<-max(sqrt(line.vals.short ^2))+0.001
			cut.vals<-seq(-max.val, max.val, length.out=n.lines+1)
			if(any(line.vals==Inf)){cut.vals[length(cut.vals)]<-Inf}
			if(any(line.vals==-Inf)){cut.vals[1]<-(-Inf)}	
			line.cols<-brewer.pal.safe(n.lines, "RdBu", type="diverging")[c(n.lines:1)]
		}else{	# sequential colour palette
			cut.vals<-seq(min(line.vals.short - 0.001), max(line.vals.short + 0.001), length.out=n.lines+1)
			if(any(line.vals==Inf)){cut.vals[length(cut.vals)]<-Inf}
			if(any(line.vals==-Inf)){cut.vals[1]<-(-Inf)}	
			line.cols<-brewer.pal.safe(n.lines, "Purples", type="increasing")
		}
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

	# set NA values
	if(is.null(plot.defaults$na.control)){plot.defaults$na.control<-NA
	}else{
		if(length(plot.defaults$na.control)>1 & is.na(plot.defaults$na.control[[1]])==FALSE){
			plot.defaults$na.control<-append.missed.columns(
				plot.defaults$na.control, list(lwd=1, lty=2, col="grey50"))}
		}

	# set border for style="clock"
	border.default<-list(lwd=1, lty=1, col="grey30", tcl=-0.07)
	if(is.null(plot.defaults$border)){plot.defaults$border<-border.default
	}else{plot.defaults$border <-append.missed.columns(
		plot.defaults$border, border.default)}


	## ERROR AVOIDANCE ##
	# ensure line.breaks incapsulate all values of input
	range.input<-range(line.vals)
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


# function to properly apply line attributes to a list
clean.lines<-function(y, binary, options.list){
	if(binary){
		# remove 'absent' connections
		y<-y[which(y$value==1), ]
		if(nrow(y)==0){stop("No connections are available to draw - binary input must contain some present values")}
	}else{
		# order line list by effect size
		effect.size<-y$value^2
		y<-y[order(effect.size), ]}
	# place NA values first (so they are underneath drawn values)
	if(any(is.na(y$value))){
		y<-y[c(
			which(is.na(y$value)==TRUE), 
			which(is.na(y$value)==FALSE)), ]}
		
	# set line colours & widths. 
	line.cuts<-cut(y$value, options.list$line.breaks, include.lowest=TRUE, right=TRUE, labels=FALSE)
	line.defaults<-data.frame(
		col= options.list$line.cols[line.cuts],
		lwd= options.list$line.widths[line.cuts],
		lty=1,
		arrows=TRUE,
		stringsAsFactors=FALSE)

	# whether or not arrows should be added
	if(class(options.list$na.control)=="list"){	
		if(any(is.na(y$value))){
			line.defaults$col[which(is.na(y$value))]<-options.list$na.control$col
			line.defaults$lwd[which(is.na(y$value))]<-options.list$na.control$lwd
			line.defaults$lty[which(is.na(y$value))]<-options.list$na.control$lty
			line.defaults$arrows[which(is.na(y$value))]<-FALSE}
	}else{
		if(any(is.na(y$value))){
			keep.rows<-which(is.na(y$value)==FALSE)
			y<-y[keep.rows, ]
			line.defaults<-line.defaults[keep.rows, ]}
	}

	# add infomation from line.defaults to line.list
	keep.cols<-sapply(colnames(line.defaults), FUN=function(z){any(colnames(y)==z)})
	add.cols<-which(keep.cols==FALSE)
	add.names<-names(add.cols)
	y<-cbind(y, line.defaults[, add.cols])
	new.entries<-c((ncol(y)-length(add.cols)+1):ncol(y))
	colnames(y)[new.entries]<-add.names

	return(y)
	}



# function to take results from set.plot.attributes, and use them to create data.frames full of relevant information for plotting
calc.circleplot<-function(x, plot.options, cluster, style){

	# POINTS
	n.points<-nrow(plot.options$points)
	circle.points<-as.data.frame(make.circle(n.points, alpha= plot.options$plot.rotation)[, 2:3])

	# set distances for labels - note these must now be dependent on style
	if(style=="pie"){edge.max<-1+(plot.options$points$cex[1]*0.1)
	}else{edge.max<-1}

	if(any(colnames(plot.options$point.labels)=="offset")){
		label.distance<- edge.max + mean(plot.options$point.labels$offset, na.rm=TRUE)
	}else{label.distance<- edge.max  + 0.05}
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
	if(class(plot.options$point.labels)=="logical"){
		if(length(plot.options$point.labels)==1){
			label.dframe<-point.dframe
			label.dframe$cex<-0}
	}else{
		label.dframe<-plot.options$point.labels}

	# reorder as necessary
	if(any(grepl("order", colnames(point.dframe)))){
		order.cols<-which(grepl("order", colnames(point.dframe)))
		if(length(order.cols)>1){
			row.order<-do.call(order, as.list(point.dframe[, order.cols]))
		}else{
			row.order<-order(point.dframe[, order.cols])}
		  	point.dframe<-point.dframe[row.order, -order.cols]
		# label.dframe<-label.dframe[row.order, ]
	}else{row.order<-c(1:nrow(point.dframe))}

	# add coordinates
	point.dframe<-cbind(circle.points, point.dframe)

	# ensure point and label data.frames having matching row orders
	label.order<-sapply(point.dframe$labels, function(a, lookup){
		which(lookup$labels==a)}, lookup=label.dframe)
	label.dframe<-label.dframe[label.order, ]

	# add labels if needed
	label.suppress.test<-is.logical(plot.options$point.labels) & length(plot.options$point.labels)==1
	if(label.suppress.test==FALSE){
		label.dframe<-cbind(circle.labels[, c(1, 2, 4)], label.dframe)
		# correct label presentation
		label.dframe$srt[which(label.dframe$x<0)]<-label.dframe$srt[which(label.dframe$x<0)]+180
		label.dframe$adj<-0
		label.dframe$adj[which(label.dframe$x<0)]<-1
	}

	# export as a list
	point.list<-list(point.dframe, label.dframe)
	names(point.list)<-c("points", "labels")


	# Set styles
	if(style=="clock"){
		coord.start<-as.data.frame(
			make.circle(n.points, alpha= plot.options$plot.rotation, k=(1 + plot.options$border$tcl))[, 2:3])
		x.list<-as.list(as.data.frame(t(cbind(point.list$points$x, coord.start$x))))
			names(x.list)<-rep("x", length(x.list))
		y.list<-as.list(as.data.frame(t(cbind(point.list$points$y, coord.start$y))))
			names(y.list)<-rep("y", length(y.list))
		# data.list<-split(point.data[, -c()], c(1:nrow(point.data)))
		border.attr<-plot.options$border[-which(names(plot.options$border)=="tcl")]
		data.list<-vector("list", length(x.list)) 
		for(i in 1:length(data.list)){
			coordinates<-append(x.list[i], y.list[i])
			data.list[[i]]<-append(coordinates, border.attr)
			}
		point.list$nodes<-data.list
		# now add the border itself
		border.coords<-make.circle(100, alpha= plot.options$plot.rotation, k=1)[, 2:3]
		border.coords<-rbind(border.coords, border.coords[1, ])
		point.list$border<-append(as.list(border.coords), 
			plot.options$border[-which(names(plot.options$border)=="tcl")])
	}

	if(style=="pie"){
		# use clustering to determine whether which sets of adjacent points have unique attributes
		point.attributes<-point.list$points[, -c(1:3)]
		# convert any character data to factors
		class.list<-lapply(point.attributes, class)
		if(any(class.list =="character")){
			select<-which(class.list=="character")
			for(i in 1:length(select)){point.attributes[, select[i]]<-as.factor(point.attributes[, select[i]])}}
		# use this to calculate groups
		point.distance<-daisy(point.attributes, "gower")
		point.cluster<-hclust(point.distance)
		polygon.attributes<-point.list$points
		# attach to initial values
		if(any(point.distance>0)){
			initial.vals<-cutree(point.cluster, h=min(point.distance[which(point.distance>0)])*0.5)
			initial.vals<-initial.vals - initial.vals[1] + 1 # set 1st value to 1
			final.vals<-initial.vals
			for(i in 2:length(initial.vals)){
				if(initial.vals[i]==initial.vals[(i-1)]){final.vals[i]<-initial.vals[(i-1)]
				}else{final.vals[i]<-initial.vals[(i-1)]+1}}
			polygon.attributes$group<-final.vals
		}else{
			polygon.attributes$group<-1}
		# now go through these and determine which are different from previous
		# this works because points are drawn in row order
		polygon.group<-rep(0, nrow(polygon.attributes))
		for(i in 2:nrow(polygon.attributes)){
			if(polygon.attributes$group[i]!=polygon.attributes$group[(i-1)]){polygon.group[i]<-1
			}else{polygon.group[i]<-0}}
		polygon.attributes$group<-cumsum(polygon.group)+1
		# at this point it might be worth removing irrelevant columns (i.e. that only work on points, not polygons)
		# make a set of polygons for plotting
		polygon.list.initial<-split(polygon.attributes, polygon.attributes$group)
		n.points<-nrow(point.list$points)
		m<-10 # must be an even number
		circle.points.offset<-as.data.frame(make.circle(n.points*m, 
			alpha=plot.options$plot.rotation+(180/(n.points*m)))[, 2:3])
		circle.points.max<-as.data.frame(make.circle(n.points*m, 
			alpha=plot.options$plot.rotation+(180/(n.points*m)), k= edge.max)[, 2:3])
		colnames(circle.points.max)<-c("x.max", "y.max")
		circle.points.offset<-cbind(circle.points.offset, circle.points.max)
		circle.points.offset<-rbind(circle.points.offset[nrow(circle.points.offset), ], circle.points.offset)#, circle.points.offset[1, ])
		point.list$polygons<-lapply(polygon.list.initial, function(x, points, options, res){
			make.polygon(x, points, options, res)},
			points= circle.points.offset, options = plot.options, res=m)
		} # end pie style

	# LINES
	line.list<-lapply(x$long, function(y, binary, options.list){
		colnames(y)[3]<-"value"
		clean.lines(y, binary, options.list)}, binary=x$binary, options.list=plot.options)

	# PLOT
	x.lim<-c(min(point.dframe$x), max(point.dframe$x))
	# extra x margins added
	# note this works beacuse set.plot.attributes allows FALSE as the only logical operator to point.labels
	label.suppress.test<-is.logical(plot.options$point.labels) & length(plot.options$point.labels)==1
	if(label.suppress.test==FALSE){
		max.label<-max(nchar(point.dframe$labels))
		x.expansion<-((label.distance[1]-1)*0.5) + (max.label*0.03)
		x.lim<-colSums(rbind(x.lim, c(-x.expansion, x.expansion)))
	}else{x.lim<-c(-edge.max, edge.max)}
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


# function to offset all data in a figure by a specified amount, and/or scale size of the figure
offset.circleplot<-function(x,
	offset.x=0,
	offset.y=0,
	scale=1	# multiplier
 	){

	# start with point data
		# extract information on point locations
		point.vals<-x$locations$points
		label.vals<-x$locations$labels
		# goal is to maintain constant distance of labels from edge - extract these data
		delta.x<-label.vals$x-point.vals$x
		delta.y<-label.vals$y-point.vals$y
		# multiply by scale
		point.vals$x<-( point.vals$x * scale )
		point.vals$y<-( point.vals$y * scale )
		label.vals$x<-point.vals$x + delta.x
		label.vals$y<-point.vals$y + delta.y
		# offset x & y
		point.vals$x<-( point.vals$x + offset.x )
		point.vals$y<-( point.vals$y + offset.y )
		label.vals$x<-( label.vals$x + offset.x )
		label.vals$y<-( label.vals$y + offset.y )	
		# put back in to original lists
		x$locations$points<-point.vals
		x$locations$labels<-label.vals

	# internal functions
	adjust.locations<-function(y, ax, ay, mu){
		y$x <- y$x * mu
		y$y <- y$y * mu
		y$x <- y$x + ax
		y$y <- y$y + ay
		return(y)}
	adjust.lines<-function(z, add.x, add.y, multiply){
		lapply(z, function(z, ax, ay, mu){adjust.locations(z, ax, ay, mu)},
		ax= add.x, ay= add.y, mu= multiply)}

	# adjust remaining content
	# lines
	x$line.data<-lapply(x$line.data, function(z, add.x, add.y, multiply){
		adjust.lines(z, add.x, add.y, multiply)},
		add.x=offset.x, add.y=offset.y, multiply=scale)

	# borders (style="clock")
	if(any(names(x$locations)=="border")){
		x$locations$border<-adjust.locations(x$locations$border, offset.x, offset.y, scale)}

	if(any(names(x$locations)=="nodes")){
		x$locations$nodes<-lapply(x$locations$nodes, function(y, ax, ay, mu){
			adjust.locations(y, ax, ay, mu)},
			ax= offset.x, ay= offset.y, mu= scale)}	

	# polygons (style="pie")
	if(any(names(x$locations)=="polygons")){
		x$locations$polygons<-lapply(x$locations$polygons, function(y, ax, ay, mu){
			adjust.locations(y, ax, ay, mu)},
			ax= offset.x, ay= offset.y, mu= scale)}	

	return(x)
	}