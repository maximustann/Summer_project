source("pre.R")
library(foreach)
library(GA)
library(mco)
library(nsga2R)
algorithm <- function(services, candidate_cities){
	#	assume candidate_cities and services are lists
	#	first get the number of candidate_cities and services
	#	The population size is 30
	#	We only have two objectives, so objDim = 2
	#	varNo is the variable Number, we have 12 = 3 rows * 4 candidate cities
	#	Mutation distribution index, don't know how it works yet*
	#	Crossover distribution index, don't know how it works either*
	#	cprob is Crossover probability
	#	mprob is Mutation probability
#======================initialize parameters==================================
	candidate_cities_num <- length(candidate_cities)
	services_num <- length(services)
	popSize <- 30
	objDim <- 2		
	varNo <- services_num * candidate_cities_num
	tourSize <- 10
	MutDistIdx <- 20
	mprob <- 0.2
	XoverDistIdx <- 20
	cprob <- 0.7
	generations <- 50
#=============================================================================

#========================Algorithm Starts=====================================
	#	step 1, initialize the population
	pop <- generate_population(candidate_cities_num, services_num, popSize)
	#print(pop)






#====================unNormalized Fitness================================
	#	step 2, calculate the fitness
	#unNormalized <- t(apply(pop, 1, fitness))
	#pop <- cbind(pop, normalize(unNormalized))
	pop <- cbind(pop, t(apply(pop, 1, fitness)))
	#origin_range <- range(c(min(pop[, varNo + 1]), max(pop[, varNo + 1])))
	origin_range_y <- range(0, max(pop[, varNo + 2]))
	origin_range_x <- range(0, max(pop[, varNo + 1]))
	plot(pop[, (varNo + 1):(varNo + objDim)], xlim = origin_range_x, ylim = origin_range_y, col = 'blue', xlab = 'cost', ylab = 'latency')


	#	step 3, rank it
	ranking <- fastNonDominatedSorting(pop[, (varNo + 1):(varNo + objDim)])
	#print(ranking)
	rnkIndex <- integer(popSize)
	i <- 1
	while (i <= length(ranking)){
		rnkIndex[ranking[[i]]] <- i
		i <- i + 1
	}
	pop <- cbind(pop, rnkIndex)

	
	#	step 4, calculate the crowding value
	objRange <- apply(pop[, (varNo + 1) : (varNo + objDim)], 2, max) - apply(pop[, (varNo + 1) : (varNo + objDim)], 2, min)
	cd <- crowdingDist4frnt(pop, ranking, objRange)

	#	step 5, selection
	pop <- cbind(pop, apply(cd, 1, sum))


	for(iter in 1:generations){
		matingPool <- tournamentSelection(pop, popSize, tourSize)
		#print(matingPool)
	
		#	step 6, Crossover
		#mating <- apply(matingPool[, 1:varNo], 1, binary2decimal)
		#childAfterX <- boundedSBXover(matingPool[, 1:varNo], lowerBounds, upperBounds, cprob, XoverDistIdx)
		childAfterX <- crossover(matingPool[, 1:varNo], cprob)
		#print(childAfterX)
	
		#	step 7 Mutation
		#childAfterM <- boundedPolyMutation(childAfterX, lowerBounds, upperBounds, mprob, MutDistIdx)
		childAfterM <- mutation(childAfterX, mprob)
		#print(childAfterM)
		childAfterM <- cbind(childAfterM, t(apply(childAfterM, 1, fitness)))
		par(new = T)
		#print(childAfterM)
		parentNext <- rbind(pop[, 1:(varNo + objDim)], childAfterM)
		#print(parentNext)
		ranking <- fastNonDominatedSorting(parentNext[, (varNo + 1) : (varNo + objDim)])
		i <- 1
		while (i <= length(ranking)){
			rnkIndex[ranking[[i]]] <- i
			i <- i + 1
		}
		parentNext <- cbind(parentNext, rnkIndex)
		objRange <- apply(pop[, (varNo + 1) : (varNo + objDim)], 2, max) - apply(pop[, (varNo + 1) : (varNo + objDim)], 2, min)
		#print(objRange)
		cd <- crowdingDist4frnt(parentNext, ranking, objRange)
		parentNext <- cbind(parentNext, apply(cd, 1, sum))
		parentNext.sort <- parentNext[order(parentNext[, varNo + objDim + 1], -parentNext[, varNo + objDim + 2]), ]
		parent <- parentNext.sort[1:popSize, ]
		plot(parent[, (varNo + 1):(varNo + objDim)], xlim = origin_range_x, ylim = origin_range_y, col = 'red')
	}

	result = list(functions = fitness, parameterDim = varNo, objectiveDim = objDim, popSize = popSize,
				  tournamentSize = tourSize, generations = generations, XoverProb = cprob, mutationProb = mprob,
				  parameters = parent[,1:varNo], objectives = parent[, (varNo + 1):(varNo + objDim)], 
				  paretoFrontRank = parent[, varNo + objDim + 1], crowdingDistance = parent[, varNo + objDim + 2])
	class(result) = "nsga2R"
	return(result)
}

generate_population <- function(row, col, size){
	#	population is a list, every index contains a matrix, which is a chromosome
	#	col		candidate_cities:	A B C D
	#	row		services			S1 S2 S3
	#	The population looks like:
	#					A	B	C	D
	#				S1	1	0	0	1
	#				S2	0	1	0	0
	#				S3	0	0	1	0
	pop <- vector()
	set.seed(1)
	for(i in 1:size) {
		#change binary to decimal
		#pop <- rbind(pop, binary2decimal(rbinom(row * col, 1, 0.5)))
		pop <- rbind(pop, rbinom(row * col, 1, 0.5))
	}
	pop
}

fitness <- function(chromosome) {
	cost <- cost_fitness(chromosome)
	latency <- latency_fitness(chromosome)
	return(c(cost, latency))
}

crossover <- function(parent_chromosome, cprob){
	popSize = nrow(parent_chromosome)
	varNo = ncol(parent_chromosome)
	new_pop <- vector()
	p <- 1
	for (i in 1:(popSize / 2)){
		if(runif(1) < cprob){
			cutPoint <- floor(runif(1, 1, varNo))
			child_1 <- parent_chromosome[p, 1:cutPoint]
			child_1 <- c(child_1, parent_chromosome[p + 1, (cutPoint + 1):varNo])
			child_2 <- parent_chromosome[p + 1, 1:cutPoint]
			child_2 <- c(child_2, parent_chromosome[p, (cutPoint + 1):varNo])
		}
		else{
			child_1 <- parent_chromosome[p, ]
			child_2 <- parent_chromosome[p + 1, ]
		}
		new_pop <- rbind(new_pop, child_1)
		new_pop <- rbind(new_pop, child_2)
		p <- p + 2
	}
	new_pop
}

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

cost_fitness <- function(chromosome){
	#chromosome <- decimal2binary(chromosome, 12)
	chromosome <- matrix(chromosome, nrow = 3, ncol = 4)
	cost <- sum(chromosome * cost_matrix)
	cost
}

latency_fitness <- function(chromosome){
	#chromosome <- decimal2binary(chromosome, 12)
	chromosome <- matrix(chromosome, nrow = 3, ncol = 4)
	latency <- sum(chromosome * latency_matrix * frequency_matrix)
	latency
}

normalize <- function(data){
	normalized_data <- vector()
	for(i in 1:ncol(data)){
		normalized_data <- cbind(normalized_data, (data[, i] - mean(data[, i])) / sd(data[, i]))
	}
	normalized_data
}

constraint <- function(chromosome, top_cost, top_latency, frequency_matrix, cost_matrix, latency_matrix){
	cost_constraint <- chromosome * cost_matrix - top_cost
	latency_constraint <- chromosome * frequency_matrix * latency_matrix - top_latency

}
