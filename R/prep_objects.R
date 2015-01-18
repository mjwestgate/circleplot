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
	if(binary.test){	# first for binary matrices
		dist1<-as.dist(dataset +t(dataset))
		dist2<-as.dist(2*dataset)
		asymmetry.test<-any(c(dist1==dist2)==FALSE)
		if(asymmetry.test){	# if asymmetric, then it is necessary to show the direction of the effect in dist
			direction.matrix<-as.dist(dataset)-as.dist(t(dataset))
			direction.matrix[which(direction.matrix==0)]<-1
			distance.matrix<-dist1*direction.matrix
		}else{distance.matrix<-dataset*2}
	}else{	# then numeric matrices
		dist1<-as.dist(as.matrix(dataset))
		dist2<-as.dist(t(as.matrix(dataset)))
		asymmetry.test<-any(c(dist1==dist2)==FALSE, na.rm=TRUE)
		if(asymmetry.test){
			dist1[1:length(dist1)]<-apply(cbind(as.vector(dist1), as.vector(dist2)), 1, mean)}
		distance.matrix<-dist1
		}

	if(binary.test==F & asymmetry.test==T){stop('circleplot does not support numeric, asymmetric matrices (yet)')}

	# export these as a list-based S3 object that can be passed to later functions
	matrix.properties<-list(
		initial.class=class(input), 
		binary=binary.test,
		asymmetric= asymmetry.test,
		dist=distance.matrix,
		source=input
		)

	return(matrix.properties)
	}	# end function



# function to set plot defaults, and overwrite if new data is provided
set.plot.attributes<-function(
	input,	# result from check.inputs
	plot.control
	)
	{
	distance.matrix<-input$dist

	## SET DEFAULTS ##
	# generate a some default values for points
	n.points<-attr(distance.matrix, "Size")
	point.defaults<-data.frame(
			label=attr(distance.matrix, "Labels"),
			pch=19,
			col=rep(rgb(t(col2rgb("grey30")), maxColorValue=255), n.points),
			cex=2,
			stringsAsFactors=FALSE)
	rownames(point.defaults)<-point.defaults$label

	# set defaults for line cuts, colours etc - set all to grey by default
	if(input$binary){
			cut.vals<-c(-1, 2)	; line.cols<-"grey30"
	}else{	# for numeric matrices
		overlap.zero<-min(distance.matrix, na.rm=TRUE)<0 & max(distance.matrix, na.rm=TRUE)>0
		if(overlap.zero){	# diverging colour palette
			cut.vals<-c(
				seq(min(distance.matrix, na.rm=TRUE), 0, length.out=4)[1:3], 0, 
				seq(0, max(distance.matrix, na.rm=TRUE), length.out=4)[2:4])
			line.cols<-brewer.pal(6, "RdBu")[6:1]
		}else{	# sequential colour palette
			cut.vals<-seq(min(distance.matrix, na.rm=TRUE), max(distance.matrix, na.rm=TRUE), length.out=7)
			line.cols<-brewer.pal(6, "Purples")}
	}	# end colour selection			

	# make a list of point and line attributes, showing the required properties
	plot.defaults<-list(
		points=point.defaults,
		point.labels=TRUE,
		line.gradient=FALSE,	# option for binary matrices only
		line.breaks=cut.vals,
		line.cols=line.cols,
		line.widths=rep(1, length(line.cols)),
		line.expansion=0,	# formerly line.curvature
		line.curvature=c(add=0.25, multiply=0.35),	# new command to set height of each quadratic curve
		na.control=list(lwd=1, lty=2, col="grey")
		)

	## ALLOW USER SPECIFICATION ##	
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

	## ERROR AVOIDANCE ##
	# make up to two colours if asymmetry.test==TRUE, and two points have not yet been provided
	default.directional.cols<-c("grey80", "grey10")
	if(input$asymmetric){
		if(length(plot.defaults$line.cols)==1){
			if(plot.defaults$line.cols=="grey30"){plot.defaults$line.cols<-default.directional.cols
			}else{plot.defaults$line.cols<-c(default.directional.cols[1], plot.defaults$line.cols)}}}

	# correct line.width if necessary
	# only change if defaults have been overwritten with poor inputs
	if(length(plot.defaults$line.widths)!=(length(plot.defaults$line.breaks)-1)){	
		if(length(plot.defaults$line.widths)==1){
			plot.defaults$line.widths<-rep(plot.defaults$line.widths, length(line.cols))
		}else{if(length(plot.defaults$line.widths)>1){	# i.e. if a min and max is given, choose only the max value
			plot.defaults$line.widths<-rep(max(plot.defaults$line.widths, na.rm=TRUE), length(line.cols))
		}}}

	# Invert direction of line.expansion
	plot.defaults$line.expansion<-1-plot.defaults$line.expansion
	# This is a bit pointless, but is included for consistency with earlier versions.

	# if line.curvature is a list, convert to vector	
	if(is.list(plot.defaults$line.curvature)){plot.defaults$line.curvature<-unlist(plot.defaults$line.curvature)}

	# if point.attr are <v0.3 standard, convert to new format (i.e. for passing to do.call(points, ...)
	if(any(colnames(plot.defaults$points)=="colour")){
		x<-which(colnames(plot.defaults$points)=="colour")
		colnames(plot.defaults$points)[x]<-"col"}
	if(any(colnames(plot.defaults$points)=="size")){
		x<-which(colnames(plot.defaults$points)=="size")
		colnames(plot.defaults$points)[x]<-"cex"}
	if(any(colnames(plot.defaults$points)=="pch")==FALSE){
		plot.defaults$points$pch<-rep(19, dim(plot.defaults$points)[1])}
	# ensure that labels are characters, not vectors
	plot.defaults$points$label<-as.character(plot.defaults$points$label)

	return(plot.defaults) # return result
	}
	


# get binary data into an appropriate format for plotting
# This has been replaced by a direct call to inner.circle() for now
prep.binary<-function(
	dataset,
	plot.control,
	cluster
	)
	{
	point.names<-attr(dataset$dist, "Labels")
	if(any(is.na(as.numeric(dataset$dist))))cluster<-FALSE

	# make points for plotting
	circle.points<-as.data.frame(make.circle(attr(dataset$dist, "Size"))[, 2:3])

	# reorder (or not)
	if(cluster){
		cluster.result<-hclust(dataset$dist)
		circle.points$label<-point.names[cluster.result$order]
	}else{circle.points$label<-point.names}
	circle.points$label<-as.character(circle.points$label)
	circle.points<-circle.points[order(circle.points$label), ]

	# now data.frame where each row shows a line
	line.list<-as.data.frame(cbind(
		t(combn(point.names, 2)), 
		as.numeric(dataset$dist)))
		colnames(line.list)<-c("sp1", "sp2", "value")
		for(i in 1:2){line.list[, i]<-as.character(line.list[, i])}
		line.list$value<-as.numeric(as.character(line.list$value))

	# remove 'absent' connections
	line.list<-line.list[-which(line.list$value==0), ]

	# place NA values first (so they are underneath drawn values)
	line.list<-line.list[c(which(is.na(line.list$value)==TRUE), 
		which(is.na(line.list$value)==FALSE)), ]

	# add attributes to circle locations
	circle.points<-merge(circle.points, plot.control$points, by="label")
		# circle.points<-circle.points[, c(2, 3, 1, 4:dim(circle.points)[2])]
		rownames(circle.points)<-circle.points$label

	# export
	return(list(points=circle.points, lines=line.list))
	}




# function to prepare data for analysis if input matrix is numeric
prep.numeric<-function(
	dataset, 
	plot.control,
	cluster
	)
	{
	point.names<-attr(dataset$dist, "Labels")
	if(any(is.na(as.numeric(dataset$dist))))cluster<-FALSE

	# point info prep 
	circle.points<-as.data.frame(make.circle(attr(dataset$dist, "Size"))[, 2:3])

	# work out point order using clustering (or not)
	if(cluster){
		connection.distance<-as.dist(1-(sqrt(dataset$dist^2)))
		result<-hclust(connection.distance)
		circle.points$label<-point.names[result$order]
	}else{
		circle.points$label<-point.names}

	# add point attributes
	circle.points<-merge(circle.points, plot.control$points, by="label")
		# circle.points<-circle.points[, c(2, 3, 1, 4:dim(circle.points)[2])]
		rownames(circle.points)<-circle.points$label

	# line info prep
	line.list <- data.frame(t(combn(point.names, 2)),
			as.vector(dataset$dist),
                        stringsAsFactors = FALSE)
	colnames(line.list)<-c("sp1", "sp2", "value")

	# order line list by effect size
	effect.size<-line.list$value^2
	line.list<-line.list[order(effect.size), ]

	# place NA values first (so they are underneath drawn values)
	line.list<-line.list[c(which(is.na(line.list$value)==TRUE), 
		which(is.na(line.list$value)==FALSE)), ]

	return(list(points=circle.points, lines=line.list))
	}
