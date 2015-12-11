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


# Functions on lists

# take a list containing co-occurrence data, and return a list of the same length, 
# but with only those species shared among all datasets (type="AND") 
# or all species present in any dataset (type="OR", the default)
clean.list<-function(x, type="OR"){
	if(any(c("AND", "OR")==type)==FALSE)stop("Specified 'type' not permitted; please specify AND or OR")
	# first ensure that data are in the same (wide) format
	x<-lapply(x, function(y){
		if(class(y)=="data.frame"){y<-make.wide.format(y)}else{y<-as.matrix(y)}})
	n<-length(x)
	comparison<-calc.overlap(x)
	if(type=="OR"){
		all.species<-rownames(comparison)
		for(i in 1:n){
			y<-x[[i]]
			missing.rows<-which(comparison[, i]==FALSE)
			new.cols<-matrix(data=NA, nrow=nrow(y), ncol=length(missing.rows))
				colnames(new.cols)<-all.species[missing.rows]
			output <-cbind(y, new.cols)
			new.rows<-matrix(data=NA, nrow=length(missing.rows), ncol=ncol(output))
				rownames(new.rows)<-all.species[missing.rows]
			output <-rbind(output, new.rows)
			col.order<-order(colnames(output))
			x[[i]]<-output[col.order, col.order]	
		}
	}else{
		and.test<-apply(comparison, 1, FUN=function(y){any(y==FALSE)==FALSE})
		keep.rows<-which(and.test)
		if(length(keep.rows)==0){stop("No species are present in all datasets; try type='OR'")
		}else{
			all.species<-rownames(comparison)[keep.rows]
			for(i in 1:n){
				y<-x[[i]]
				keep.cols<-which(sapply(colnames(y), 
					FUN=function(z, comp){any(comp==z)}, comp=all.species))
				output<-y[keep.cols, keep.cols]
				col.order<-order(colnames(output))
				x[[i]]<-output[col.order, col.order]			
			}		
	}}
	return(x)
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