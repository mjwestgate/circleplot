## FUNCTIONS TO PROCESS DATA
# Note: these are currently identical to functions in sppairs, but are duplicated here to reduce dependencies.

# function to make a square matrix from a data.frame
make.wide.format<-function(
	input	# result from spaa()
	){
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
