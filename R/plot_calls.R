# plot functions - called by user

# function to use the above code to draw a figure
circleplot<-function(
	input,	# a distance matrix (class 'dist') or square matrix (class matrix)
	plot.control	# a matrix containing plot attributes. See ?circleplot
	)
	{
	# initial processing
	dataset<-check.inputs(input)
	plot.control<-set.plot.attributes(dataset, plot.control) # set plot attributes/defaults

	# run appropriate prep code
	if(dataset$binary){	# if binary
		result<-prep.binary(dataset, plot.control) # prep.binary(dataset, plot.control$points)
	}else{
		result<-prep.numeric(dataset, plot.control)}

	# call plot code
	par(mar=rep(0.5, 4))	# set window attributes
	plot(x= result$points$x, y= result$points$y, type="n", ann=FALSE, axes=FALSE, asp=1)	# plot
	draw.curves(dataset, result, plot.control) 	# add lines
	points(result$points$x, result$points$y, 	# add points
		pch=19, 
		col= result$points$colour, 
		cex= result$points$size)
	text(result$points$x, result$points$y, label= result$points$label, col="white", cex=0.7)	# label points

	# if(singletons)add.outer.points
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
			colour=color.hex,
			size=rep(3, length(labels)),
			stringsAsFactors=FALSE)
	}