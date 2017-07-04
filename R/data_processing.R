## FUNCTIONS TO PROCESS DATA
# Note: these are currently identical to functions in sppairs, but are duplicated here to reduce dependencies.

# function to make a square matrix from a data.frame
make.wide.format<-function(
	input	 # result from spaa()
	){
	if(class(input)!="data.frame"){stop("make.wide.format only works for class(input)=='data.frame'")}
	# work out properties of the input
	spp.names<-unique(c(input[, 1], input[, 2]))
	n.spp<-length(spp.names)
	if(nrow(input)==choose(n.spp, 2)){asymmetric<-FALSE}else{asymmetric<-TRUE}
	# create a matrix
	result<-matrix(data=NA, nrow= n.spp, ncol= n.spp)
	colnames(result)<-spp.names
	rownames(result)<-spp.names
	# fill with a loop
	for(i in 1:nrow(input)){
		sp1<-input[i, 1]
		sp2<-input[i, 2]
		row.i<-which(spp.names==sp2)
		col.i<-which(spp.names==sp1)
		if(asymmetric){
			result[row.i, col.i]<-input[i, 3]
		}else{
			result[row.i, col.i]<-input[i, 3]
			result[col.i, row.i]<-input[i, 3]}
	}
	rownames(result)<-spp.names
	colnames(result)<-spp.names
	return(result)
	}


# function to make a 3-column data.frame from a square matrix (i.e. inverse of make.wide.format)
make.long.format<-function(input){
	if(class(input)!="matrix"){stop("make.wide.format only works for class(input)=='matrix'")}
	# get basic summaries
	asymmetric<-any(c(input==t(input))==FALSE, na.rm=TRUE)
	if(length(colnames(input))==0){spp.names<-paste("V", c(1:ncol(input)), sep="")
		}else{spp.names<-colnames(input)}
	n.spp<-ncol(input)
	# generate an appropriately-sized data.frame for the matrix in question, fill with data	
	if(asymmetric){
		line.list<-rbind(t(combn(spp.names, 2)), t(combn(spp.names, 2))[, c(2, 1)],
			matrix(rep(spp.names, each=2), nrow= n.spp, ncol=2, byrow=TRUE))
		order.list<-rbind(
			t(combn(c(1: n.spp), 2)), 
			t(combn(c(1: n.spp), 2))[, c(2, 1)],
			matrix(rep(c(1: n.spp), each=2), nrow= n.spp, ncol=2, byrow=TRUE))
		line.list<-as.data.frame(line.list[order(order.list[, 1], order.list[, 2]), ], stringsAsFactors=FALSE)
		line.list$value<-as.numeric(input)
	}else{
		line.list<-data.frame(t(combn(spp.names, 2)), stringsAsFactors=FALSE)
		line.list$value<-as.numeric(as.dist(input))}
	# clean results
	colnames(line.list)[1:2]<-c("sp1", "sp2") # good colnames
	line.list<-line.list[which(c(line.list$sp1!=line.list$sp2)), ] # remove diagonals
	line.list<-line.list[order(line.list$sp1, line.list$sp2), ] # consistent order
	return(line.list) # export
	}


# function to take an input (preferably in long format) and return a sensible distance matrix
make.dist.format<-function(input){
	# get objects
	if(any(c("matrix", "data.frame")==class(input))==FALSE){
		stop("make.dist.format only accepts class matrix or data.frame")}
	if(class(input)=="matrix"){
		wide<-input
		long<-make.long.format(input)}
	if(class(input)=="data.frame"){
		wide<-make.wide.format(input)
		long<-input}
	if(all(is.na(long[, 3]))){
		result<-as.dist(wide)
		result[1:length(result)]<-0
		asymmetric<-FALSE
	}else{ # i.e. if this column contains any information at all
		# remove infinite values
		if(any(long[, 3]==Inf, na.rm=TRUE)){
			replace.locs<-which(long[, 3]==Inf)
			replace.vals<-max(long[-replace.locs, 3], na.rm=TRUE)*2
			long[replace.locs, 3]<-replace.vals}
		if(any(input[, 3]==-Inf, na.rm=TRUE)){
			replace.locs<-which(long[, 3]==-Inf)
			replace.vals<-min(long[-replace.locs, 3], na.rm=TRUE)
			if(replace.vals<0){replace.vals<-replace.vals*2}else{replace.vals<-replace.vals*0.5}
			long[replace.locs, 3]<-replace.vals}
		# make +ve definite
		if(min(long[, 3], na.rm=TRUE)<0){
			long[, 3]<-long[, 3]-min(long[, 3], na.rm=TRUE)}
		# invert to make into a distance
		long[, 3]<-max(long[, 3], na.rm=TRUE)-long[, 3]
		# convert to matrix, check for asymmetry
		asymmetric<-all(wide==t(wide), na.rm=TRUE)==FALSE
		if(asymmetric){
			wide.array<-array(data=NA, dim=c(dim(wide), 2))
			wide.array[,,1]<-wide
			wide.array[,,2]<-t(wide)
			wide.array<-apply(wide.array, c(1, 2), sum)
			colnames(wide.array)<-colnames(wide)
			rownames(wide.array)<-rownames(wide)
			result<-as.dist(wide.array)
		}else{
			result<-as.dist(wide)}
		# set na values to the mean (i.e. no effect on clustering)
		if(any(is.na(result))){result[which(is.na(result))]<-mean(result, na.rm=TRUE)}
	}
	return(list(asymmetric= asymmetric, dist.matrix=result))
	}


# take a list containing co-occurrence data, and return a list of the same length, 
# but with only those species shared among all datasets (type="AND") 
# or all species present in any dataset (type="OR", the default)
clean.list<-function(x, reduce=FALSE){
	# first ensure that data are in the same (wide) format
	x<-lapply(x, function(y){
		if(class(y)=="data.frame"){y<-make.wide.format(y)}else{y<-as.matrix(y)}})
	n<-length(x)
	comparison<-calc.overlap(x)
	if(reduce){
		and.test<-apply(comparison, 1, FUN=function(y){any(y==FALSE)==FALSE})
		keep.rows<-which(and.test)
		if(length(keep.rows)==0){stop("No species are present in all datasets; try reduce=FALSE")}
		all.species<-rownames(comparison)[keep.rows]
	}else{all.species<-rownames(comparison)}

	# set up a matrix that will be filled with data for entry in x
	nspp<-length(all.species)
	empty.matrix<-matrix(data=NA, nspp, nspp)
		colnames(empty.matrix)<-all.species; rownames(empty.matrix)<-all.species

	# return a list of matrices, each containing all.species in the same order.
	result<-lapply(x, function(y, fill){
		spp<-rownames(fill)
		locations<-sapply(spp, function(z, comp){
			if(any(comp==z)){return(which(comp==z))}else{return(NA)}}, 
			comp=rownames(y))
		initial.list<-as.list(as.data.frame(y))
		filled.list<-lapply(initial.list, function(z, lookup){z[lookup]}, lookup=locations)
		empty.list <-as.list(as.data.frame(fill))
		final.list<-append(filled.list, empty.list[which(is.na(locations))])
		order.final<-sapply(names(final.list), function(z, lookup){
			which(lookup==z)}, lookup=colnames(fill))
		final.matrix<-as.matrix(as.data.frame(final.list[order.final]))
		rownames(final.matrix)<-spp
		colnames(final.matrix)<-spp
		return(final.matrix)
		}, fill= empty.matrix)

	# clustering stage to go here - if turned off, test for identical rownames, and if missing, switch to alphabetical
	# perhaps convert list to array, use apply(result, c(1, 2), sum) to get clustering
	result.array<-array(unlist(result), dim=c(nspp, nspp, length(result)), 
		dimnames=list(all.species, all.species, names(result))) 
	result.sum<-apply(result.array, c(1, 2), function(z){sum(z, na.rm=TRUE)})
	result.dist<-make.dist.format(result.sum)	
	# return in correct format
	return(list(
		wide=result,
		long=lapply(result, make.long.format),
		distance= result.dist$dist.matrix,
		asymmetric= result.dist$asymmetric))
	}


# calculate which species are present in each dataset within a list, and return the result as a data.frame
calc.overlap<-function(x){
	# first ensure that data are in the same (wide) format
	x<-lapply(x, function(y){
		if(class(y)=="data.frame"){y<-make.wide.format(y)}else{y<-as.matrix(y)}})
	# get list of species names
	species.lists<-lapply(x, FUN=function(x){colnames(x)})
	all.species<-unique(unlist(species.lists))
	# calculate which species are present in each dataset
	result<-lapply(x, FUN=function(y, comp){
		sapply(comp, FUN=function(z, this.list){
			if(any(this.list ==z)){return(TRUE)}else{return(FALSE)}}, this.list =colnames(y))
		}, comp=all.species)
	as.data.frame(result)
	}