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
	# if(missing(alpha))alpha<-22.91	# as original default was 0.4 radians
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
	multiplier<-sign(mean.point[1]); if(multiplier==0)multiplier<-1
	result<-data.frame(
		x=c(mean.point[1], as.numeric(mean.point[1]-(adj*multiplier))), 
		y=c(mean.point[2], as.numeric(mean.point[2]-(opp*multiplier))))
		# note: *sign() necessary to avoid -ve x vals giving apex(s) that are outside of the circle
	rownames(result)<-c("mean", "apex")
	output<-list(as.numeric(angle2), result)
		names(output)<-c("angle", "coordinates")
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
	curve,	# data.frame returned by fit.circle
	apex		# list returned by curve.apex
	)
	{
	adjusted.angle<-apex$angle-(90*pi/180)	# because your curve faces down, not right
	curve$y<-curve$y-curve$y[51]	# set apex =0,0
	if(sqrt(apex$coordinates$x[2]^2)<10^-10){	# apex x value close to zero
		if(apex$coordinates$y[2]>0){curve$y<-(-curve$y)}
		curve.new<-curve
		# no adjustment required for y<0
	}else{	# i.e. not directly above or below origin
		if(apex$coordinates$x[2]<0){
			x.new<-(curve$x*cos(adjusted.angle))-(curve$y*sin(adjusted.angle))	# calculate transformation
			y.new<-(curve$x*sin(adjusted.angle))+(curve$y*cos(adjusted.angle))
		}else{
			x.new<-(curve$x*cos(adjusted.angle))+(curve$y*sin(adjusted.angle))	# calculate transformation
			y.new<-(curve$x*sin(adjusted.angle))-(curve$y*cos(adjusted.angle))
		}
		curve.new<-data.frame(x=x.new, y=y.new)		# put in new dataframe
	}
	curve.new$x<-curve.new$x+apex$coordinates$x[2]	# position to new x,y
	curve.new$y<-curve.new$y+apex$coordinates$y[2]	
	return(curve.new)
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