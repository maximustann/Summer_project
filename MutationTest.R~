mutation <- function(parent_chromosome, mprob){
	popSize = nrow(parent_chromosome)
	varNo = ncol(parent_chromosome)
	new_pop <- vector()
	for(i in 1:popSize){
		child <- parent_chromosome[i, ]
		for(j in 1:varNo){
			if(runif(1) < mprob){
				child[j] <- !child[j]
			}
		}
		new_pop <- rbind(new_pop, child)
	}
	new_pop
}
