# function for determining how points should be arranged on the circumference of a circle
circle.point.arrangement<-function(dataset,
	type	# how to arrange point; either 'frequency' or 'cluster' (the default)
	){
	# prepare objects and defaults
	if(missing(type))type<-"cluster"
	circle.points<-as.data.frame(make.circle(dim(dataset)[1])[, 2:3])

	# work out order that points should be plotted in
	switch(type,
		frequency={
			circle.sums<-apply(dataset, 2, sum)
			circle.sums<-sort(circle.sums, decreasing=TRUE)
			odd.even<-c(1:length(circle.sums)) %% 2
			final.order<-c(sort(circle.sums[odd.even==1], decreasing=TRUE), sort(circle.sums[odd.even==0]))
			circle.points$label<-names(final.order)
			},
		cluster={
			#library(ape)
			connection.distance<-as.dist(1-dataset) #, method="binary")
			result<-ape::hclust(connection.distance)
			circle.points$label<-colnames(dataset)[result$order]
			}
		)

	circle.points<-circle.points[order(circle.points$label), ]
	return(circle.points)
	}	# end function



# internal function for arranging points for plotting binary matrices 
inner.circle<-function(
	dataset,
	point.attributes,
	simple
	)
	{
	# set behaviour for included points
	if(simple){min.value<-0}else{min.value<-1}

	# subset to only points >min mentions
	keep.rows<-as.numeric(which(apply(dataset, 1, sum)>min.value))
	dataset<-dataset[keep.rows, keep.rows]
	dataset.dist<-as.dist(dataset)

	# if point attributes are set by default, these will be too large; reduce to appropriate size
	if(dim(point.attributes)[1]>dim(dataset)[1]){
		point.attributes<-merge(point.attributes, 
			data.frame(label=attr(dataset.dist, "Labels"), stringsAsFactors=FALSE),
			by="label", all=FALSE)
		rownames(point.attributes)<-point.attributes$label
		}

	# make points for plotting
	circle.points<-circle.point.arrangement(dataset)

	# now make line dataset to allow drawing of lines
	point.connections<-as.dist(dataset)
	point.names<-attr(point.connections, "Labels")
	line.list<-as.data.frame(cbind(t(combn(point.names, 2)), as.vector(point.connections)))
		colnames(line.list)<-c("sp1", "sp2", "value")
	line.list<-line.list[line.list$value==1, ]
	line.list$value<-as.numeric(as.character(line.list$value))

	# add attributes to circle locations
	circle.points<-merge(circle.points, point.attributes, by="label")
		circle.points<-circle.points[, c(2, 3, 1, 4, 5)]
		rownames(circle.points)<-circle.points$label

	# export
	return(list(points=circle.points, lines=line.list))
	}



# anouther function for plotting binary matrices - NOT IMPLEMENTED YET
outer.circle<-function(
	dataset,	# simple conversion from an adjavency matrix
	initial.points	# result from inner.circle()
	)
	{
	excluded.rows<-as.numeric(which(apply(dataset, 1, sum)==1))
	n.points<-length(excluded.rows)

	# look at excluded information - i.e. those with only one connection
	cols.thisrun<-apply(dataset[excluded.rows, ], 1, function(x){which(x>0)})
	single.connections<-data.frame(
		initial=colnames(dataset)[cols.thisrun],
		final=names(cols.thisrun), stringsAsFactors=FALSE)

	# convert this to a frequency table
	reduced.points<-xtabs(rep(1, dim(single.connections)[1])~ single.connections$initial)
	reduced.points<-data.frame(
		label=names(reduced.points),
		freq=as.numeric(reduced.points))
	reduced.points<-reduced.points[order(reduced.points$freq, decreasing=TRUE), ]

	# work out how large the new circle needs to be not to look out of place
	interpoint.dist<-min(dist(initial.points$points[, 1:2]))
	n.total<-ceiling((pi*3)/interpoint.dist)
	if(n.total>n.points){final.n<-n.total}else{final.n<-n.points}
	circle2<-make.circle(n.total, k=1.5)[, 2:3]
		circle2$label.inital<-rep("none", dim(circle2)[1])
		circle2$label.final<-rep("none", dim(circle2)[1])
		circle2$allocated<-rep(0, dim(circle2)[1])

	# determine which points in circle2 should be assigned to each final point
	for(i in 1:dim(reduced.points)[1])
		{
		initial.thisrun<-which(initial.points$points$label==reduced.points$label[i])
		values<-initial.points$points[initial.thisrun, ]
		
		# work out which (remaining) points are closest
		point.distances<-rep(NA, final.n)
		for(j in 1:length(point.distances)){
			point.distances[j]<-sqrt((circle2$x[j]-values$x)^2 + (circle2$y[j]-values$y)^2)}
		points.thisrun<-order(point.distances[which(circle2$allocated==0)], 
			decreasing=TRUE)[1: reduced.points$freq[i]]

		# export this information
		circle2$allocated[points.thisrun]<-1
		circle2$label.inital[points.thisrun]<-values$label
		circle2$label.final[points.thisrun]<-single.connections$final[which(single.connections$initial==values$label)]
		}	#end i

	# remove irrelevant rows/columns from circle2
	circle2<-circle2[which(circle2$allocated==1), 1:4]
	return(list(points=circle2, frequencies= reduced.points))
	}


# get binary data into an appropriate format for plotting
# this is called by plot.curves() via draw.circle(), and uses draw.curves() to actually add the segments. 
prep.binary<-function(
	adjacency.matrix, 
	point.attributes,
	simple	# should this be a simple (1 circle) or complex (2 circles) plot? 2 circle version incomplete
	)
	{
	if(missing(simple))simple<-TRUE
	# take adjacency matrix, identify groups with many connections
	dataset<-as.matrix(adjacency.matrix)
	# apply(dataset, 1, sum)==apply(dataset, 2, sum) # can use row or col sums

	# calculate points/lines
	result<-inner.circle(dataset, point.attributes, simple)
	if(simple==FALSE){
		added.points<-outer.circle(dataset, initial.points)
		result$outer.circle=added.points$points
		result$outer.freq=added.points$freq
		}

	return(result)
	}	# end function



# function to prepare data for analysis if input matrix is numeric
prep.numeric<-function(
	numeric.matrix, 
	point.attributes
	)
	{
	point.names<-attr(numeric.matrix, "Labels")	

	# point info prep (Note: could add a clustering algorithm here to better represent inter-point relationships)
	circle.points<-as.data.frame(make.circle(attr(numeric.matrix, "Size"))[, 2:3])
	# work out point order using clustering
	#library(ape)
	connection.distance<-as.dist(1-(sqrt(numeric.matrix^2)))
	result<-ape::hclust(connection.distance)
	circle.points$label<-point.names[result$order]

	# add point attributes
	circle.points<-merge(circle.points, point.attributes, by="label")
		circle.points<-circle.points[, c(2, 3, 1, 4, 5)]
		rownames(circle.points)<-circle.points$label

	# line info prep
	line.list<-as.data.frame(cbind(t(combn(point.names, 2)), as.vector(numeric.matrix)), 
		stringsAsFactors=FALSE)
	colnames(line.list)<-c("sp1", "sp2", "value")
	line.list$value<-as.numeric(line.list$value)

	# order line list by effect size
	effect.size<-line.list$value^2
	line.list<-line.list[order(effect.size), ]

	return(list(points=circle.points, lines=line.list))
	}