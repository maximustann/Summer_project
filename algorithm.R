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

	candidate_cities_num <- length(candidate_cities)
	services_num <- length(services)
	popSize <- 30
	objDim <- 2		
	varNo <- services_num * candidate_cities_num
	tourSize <- 10
	MutDistIdx <- 20
	#upperBounds <- rep(4095, 30)
	#upperBounds <- rep(1, 30)
	#lowerBounds <- rep(0, 30)
	mprob <- 0.2
	XoverDistIdx <- 20
	cprob <- 0.7

	#	step 1, initialize the population
	pop <- generate_population(candidate_cities_num, services_num, popSize)
	#print(pop)


	#	step 2, calculate the fitness
	#unNormalized <- t(apply(pop, 1, fitness))
	#pop <- cbind(pop, normalize(unNormalized))
	pop <- cbind(pop, t(apply(pop, 1, fitness)))
	#	step 3, rank it
	ranking <- fastNonDominatedSorting(pop[, (varNo + 1):(varNo + objDim)])
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
	print(pop)
	matingPool <- tournamentSelection(pop, popSize, tourSize)
	print(matingPool)

	#	step 6, Crossover
	#mating <- apply(matingPool[, 1:varNo], 1, binary2decimal)
	childAfterX <- boundedSBXover(matingPool[, 1:varNo], lowerBounds, upperBounds, cprob, XoverDistIdx)
	#print(childAfterX)

	#	step 7 Mutation
	#childAfterM <- boundedPolyMutation(childAfterX, lowerBounds, upperBounds, mprob, MutDistIdx)
	#print(childAfterM)
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
