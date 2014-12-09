crossover <- function(parent_chromosome, cprob){
	popSize = nrow(parent_chromosome)
	varNo = ncol(parent_chromosome)
	p <- 1
	new_pop <- vector()
	for(i in 1:(popSize / 2)){
		if(runif(1) < cprob){
			cutPoint <- floor(runif(1, 1, varNo))
			child1 <- parent_chromosome[p, 1:cutPoint]
			child1 <- c(child1, parent_chromosome[p + 1, cutPoint:varNo])
			child2 <- parent_chromosome[p + 1, 1:cutPoint]
			child2 <- c(child2, parent_chromosome[p, cutPoint:varNo])
		}
		else{
			child1 <- parent_chromosome[p]
			child2 <- parent_chromosome[p + 1]
		}
		new_pop <- rbind(new_pop, child1)
		new_pop <- rbind(new_pop, child2)
		p <- p + 2
	}
	new_pop
}
