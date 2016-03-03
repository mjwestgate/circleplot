# trigonometric functions for circleplot

# make a circle of specified size
make.circle<-function(
	n,	# number of points, equally spaced around the edge of a circle
	alpha,	# offset angle, in degrees
	k)	# scaling value - larger for bigger circles. defaults to 1
	{
	if(missing(k))k<-1
	base.alpha<-(-90-(180/n))
	if(missing(alpha)){alpha<-base.alpha}else{alpha<-alpha+base.alpha}
	alpha<-alpha*(pi/180) # convert to radians
	# get coordinates
	theta<-(2*(pi/n)*seq(0, (n-1)))-alpha
	values<-data.frame(
		theta=theta,
		x=k*cos(theta),
		y=k*sin(theta))
	# reorder such that circle is drawn clockwise from the top
	values<-values[c(nrow(values):1), ]
	rownames(values)<-c(1:nrow(values))
	return(values)
	}



# draw a circle with specified origin, circumference and attributes
draw.circle<-function(k, x0=0, y0=0, alpha=0, filled=FALSE, n=100, trim=0, ...){
	data<-make.circle(n=n, alpha= alpha, k=k)[, 2:3]
	data$x<-data$x + x0
	data$y<-data$y + y0
	data<-rbind(data, data[1, ])
	if(trim>0){
		trim.start<-c(1:trim)
		trim.end<-c((nrow(data)-trim) : nrow(data) )
		data<-data[-c(trim.start, trim.end), ]}
	if(filled){polygon(data, ...)
	}else{lines(data, ...)}
	return(invisible(data))
	} 


# calculate the attributes of a triangle linking two points on the circumference and a point bisecting them, 
	# pc.scale gives the proportion of the distance between the base line and the origin
# line linking the two points is taken to be horizontal
triangle.coords<-function(coords, pc.scale=0.5)
	{
	radius<-sqrt(coords$x[1]**2 + coords$y[1]**2)
	base.length<-sqrt((coords$x[1]-coords$x[2])**2 + (coords$y[1]-coords$y[2])**2)
	adj<-base.length/2
	adj.on.radius<-adj/radius
	if(adj.on.radius<1){
		angle1<-acos(adj.on.radius)
		opp<-tan(angle1)*adj*pc.scale
	}else{opp<-0}
	coords.adjusted<-data.frame(x=c(-adj, 0, adj), y=c(0, opp, 0))
	return(coords.adjusted)
	}


# find the apex of the curve linking coords (i.e. triangle1(points)[2, ])
curve.apex<-function(coords, pc.scale=0.5)
	{
	mean.point<-c(x=mean(coords$x), y=mean(coords$y))
	angle2<-atan(mean.point[2]/mean.point[1])	# angle between 0,0 and mean.point
	#	angle2*(180/pi)
	hyp<-as.numeric(sqrt(mean.point[1]^2+ mean.point[2]^2)*pc.scale)
	adj<-as.numeric(hyp*cos(angle2))
	opp<-as.numeric(hyp*sin(angle2))
	multiplier<-sign(mean.point[1])
	if(multiplier<0.001 & multiplier>-0.001)multiplier<-1
	result<-data.frame(
		x=c(mean.point[1], as.numeric(mean.point[1]-(adj*multiplier))), 
		y=c(mean.point[2], as.numeric(mean.point[2]-(opp*multiplier))))
		# note: *sign() necessary to avoid -ve x vals giving apex(s) that are outside of the circle
	rownames(result)<-c("mean", "apex")
	output<-list(angle=as.numeric(angle2), coordinates=result)
	return(output)
	}


# find a circle that matches points given by triangle.coords() - NOT IMPLEMENTED
fit.circle<-function(coords){
	y.diff<-((coords$x[1]^2)-(coords$y[2]^2))/(2*coords$y[2])
	radius<-y.diff+coords$y[2]
	x.vals<-seq(min(coords$x), max(coords$x), length.out=101)	# make an odd number for rotation
	y.vals<-sqrt((radius^2)-(x.vals^2))
	y.vals<-y.vals-y.vals[1]
	curve.coordinates<-data.frame(x=x.vals, y=y.vals)
	return(curve.coordinates)
	}


# fit a quadratic function to points given by triangle.coords()
fit.quadratic<-function(coords)
	{
	model<-lm(y~x+I(x**2), data=coords)
	newdata<-data.frame(x=seq(min(coords$x), max(coords$x), length.out=101))
	newdata$y<-as.numeric(predict(model, newdata, se.fit=FALSE))
	return(newdata)
	}


# take curved line give by fit.quadratic(), and rotate to the angle given by curve.apex()
reposition.curve<-function(
	curve,	# data.frame returned by fit.quadratic
	apex	,	# list returned by curve.apex
	coords
	)
	{

	# set rotation behaviour
	flip.test<-c(
		any((pi*seq(0, 2, 0.5))==apex$angle), # precisely horizontal #& all(coords$y > 0),  # lying above origin
		sqrt( (apex$coordinates$x[2]^2) + (apex$coordinates$y[2]^2)) <10^-5 # close to zero
		)
	if(any(flip.test)){
		angle.list<-as.list(apex$angle - (c(0.5, 1.5) * pi))
	}else{
		angle.list<-list(apex$angle - (0.5 * pi))}

	# set apex =0,0
	curve$y<-curve$y-curve$y[51]	

	# calculate curves
	curve.list<-lapply(angle.list, function(x, curve.info, below.zero){
		if(below.zero){
			result<-data.frame(
				x=(curve.info$x*cos(x)) - (curve.info$y*sin(x)),
				y=(curve.info$x*sin(x)) + (curve.info$y*cos(x)))
		}else{
			result<-data.frame(
				x=(curve.info$x*cos(x)) + (curve.info$y*sin(x)),
				y=(curve.info$x*sin(x)) - (curve.info$y*cos(x)))
		}
		return(result)},
		curve.info=curve, below.zero=c(apex$coordinates$x[2]<=0))

	# change origin
	curve.list<-lapply(curve.list, function(x, add){x$x<-x$x+add; return(x)}, add=apex$coordinates$x[2])
	curve.list<-lapply(curve.list, function(x, add){x$y<-x$y+add; return(x)}, add=apex$coordinates$y[2])

	distance.count<-unlist(lapply(curve.list, function(x, lookup){
		new.coords<-data.frame(x=x$x-lookup$x, y=x$y-lookup$y)
		new.distances<-sqrt(new.coords$x^2 + new.coords$y^2)
		length(which(new.distances<10^-3))
		}, lookup=coords[1 ,]))

	return(as.list(curve.list[[which.max(distance.count)]]))
	}


# add polygon to edge of circle
make.polygon<-function(x, points, options, res){ #=segment.dframe, point.dframe.total, plot.options){
	min.selector<-cbind(points$x-x$x[1], points$y-x$y[1])
	row.start<-which.min(apply(min.selector, 1, function(x){sum(x^2)})) - (res/2)
	a<-nrow(x)
	min.selector<-cbind(points$x-x$x[a], points$y-x$y[a])
	row.end<-which.min(apply(min.selector, 1, function(x){sum(x^2)})) + (res/2)
	# use this to make a polygon-type data.frame
	rows<-c(row.start:row.end); inv.rows<-c(row.end:row.start)
	poly.final<-list(
		x= c(points$x[rows], points$x.max[inv.rows]),
		y= c(points$y[rows], points$y.max[inv.rows]),
		border=NA,
		col=x$col[1])
	return(poly.final)
	}