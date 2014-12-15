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
	popSize <- 100
	objDim <- 2		
	varNo <- services_num * candidate_cities_num
	tourSize <- 10
	MutDistIdx <- 20
	mprob <- 0.3
	XoverDistIdx <- 20
	cprob <- 0.7
	generations <- 50
	cost_limitation <- 700
	front <- vector()
#=============================================================================
#set.seed(1)
#========================Algorithm Starts=====================================
	#	step 1, initialize the population
	parent <- generate_population(candidate_cities_num, services_num, popSize, cost_limitation)
	#print(pop)

#====================Normalized Fitness================================
	#	step 2, calculate the fitness
	unNormalized <- t(apply(parent, 1, fitness))
	#calculate_mean_cost(unNormalized)
	parent <- cbind(parent, normalize(unNormalized))

	#pop <- cbind(pop, t(apply(pop, 1, fitness)))
	#origin_range <- range(c(min(pop[, varNo + 1]), max(pop[, varNo + 1])))
#====================Normalization Ends================================

	origin_range_y <- range(0, 1)
	origin_range_x <- range(0, 1)
	plot(parent[, (varNo + 1):(varNo + objDim)], xlim = origin_range_x, ylim = origin_range_y, col = 'blue', xlab = 'cost', ylab = 'latency')


	#	step 3, rank it
	ranking <- fastNonDominatedSorting(parent[, (varNo + 1):(varNo + objDim)])
	#print(ranking)
	rnkIndex <- integer(popSize)
	i <- 1
	while (i <= length(ranking)){
		rnkIndex[ranking[[i]]] <- i
		i <- i + 1
	}
	parent <- cbind(parent, rnkIndex)

	
	#	step 4, calculate the crowding value
	objRange <- apply(parent[, (varNo + 1) : (varNo + objDim)], 2, max) - apply(parent[, (varNo + 1) : (varNo + objDim)], 2, min)
	cd <- crowdingDist4frnt(parent, ranking, objRange)

	#	step 5, selection
	parent <- cbind(parent, apply(cd, 1, sum))
	#print(parent)

	for(iter in 1:generations){
		matingPool <- tournamentSelection(parent, popSize, tourSize)
		#print(matingPool)
	
		#	step 6, Crossover
		#mating <- apply(matingPool[, 1:varNo], 1, binary2decimal)
		#childAfterX <- boundedSBXover(matingPool[, 1:varNo], lowerBounds, upperBounds, cprob, XoverDistIdx)
		childAfterX <- crossover(matingPool[, 1:varNo], cprob)
		#print(childAfterX)
	
		#	step 7 Mutation
		#childAfterM <- boundedPolyMutation(childAfterX, lowerBounds, upperBounds, mprob, MutDistIdx)
		childAfterM <- mutation(childAfterX, mprob)
		#Check for validation
		for(j in 1:nrow(childAfterM)){
			chromosome <- services_constraint_check(childAfterM[j, ])
			if(is.logical(chromosome)){
				childAfterM[j, ] <- repair_service_minimum(childAfterM[j, ])
			}
			chromosome <- cost_constraint_check(childAfterM[j, ], cost_limitation)
			if(is.logical(chromosome)){
				#print(childAfterM[j, ])
				childAfterM[j, ] <- repair_cost_maximum(childAfterM[j, ])
				#print(childAfterM[j, ])
			}
		}

		#print(childAfterM)
		#childAfterM <- cbind(childAfterM, t(apply(childAfterM, 1, fitness)))
		
		unNormalized <- t(apply(childAfterM, 1, fitness))
		calculate_mean_cost(unNormalized)
		childAfterM <- cbind(childAfterM, normalize(unNormalized))
		#print(childAfterM)
		parentNext <- rbind(parent[, 1:(varNo + objDim)], childAfterM)
		#print(parentNext)
		ranking <- fastNonDominatedSorting(parentNext[, (varNo + 1) : (varNo + objDim)])
		i <- 1
		while (i <= length(ranking)){
			rnkIndex[ranking[[i]]] <- i
			i <- i + 1
		}
		parentNext <- cbind(parentNext, rnkIndex)
		objRange <- apply(parent[, (varNo + 1) : (varNo + objDim)], 2, max) - apply(parent[, (varNo + 1) : (varNo + objDim)], 2, min)
		#print(objRange)
		cd <- crowdingDist4frnt(parentNext, ranking, objRange)
		parentNext <- cbind(parentNext, apply(cd, 1, sum))
		parentNext.sort <- parentNext[order(parentNext[, varNo + objDim + 1]), ]
		#print(parentNext.sort)
		parent <- parentNext.sort[1:popSize, ]
		front <- parent[parent[, 15] == 1, ]
		#print(apply(front[, 1:varNo], 1, fitness))
		par(new = T)
		plot(front[, (varNo + 1):(varNo + objDim)], xlim = origin_range_x, ylim = origin_range_y, col = 'red', pch = 4)
	}
	par(new = T)
	#plot(parent[, (varNo + 1):(varNo + objDim)], xlim = origin_range_x, ylim = origin_range_y, col = 'red', pch = 3)
	plot(front[, (varNo + 1):(varNo + objDim)], xlim = origin_range_x, ylim = origin_range_y, bg = 'black', pch = 24)
	#dev.off()
	#pdf("plot.pdf")

	result = list(functions = fitness, parameterDim = varNo, objectiveDim = objDim, popSize = popSize,
				  tournamentSize = tourSize, generations = generations, XoverProb = cprob, mutationProb = mprob,
				  parameters = parent[,1:varNo], objectives = parent[, (varNo + 1):(varNo + objDim)], 
				  paretoFrontRank = parent[, varNo + objDim + 1], crowdingDistance = parent[, varNo + objDim + 2])
	class(result) = "nsga2R"
	for(iter in 1:nrow(front)){
		print(matrix(front[iter, 1:varNo], nrow = 3, ncol = 4))
	}
	#print(front)
	#return(parent)
	#return(paretoFront(result))
	#return(result)
}

generate_population <- function(row, col, size, cost_limitation){
	#	population is a list, every index contains a matrix, which is a chromosome
	#	col		candidate_cities:	A B C D
	#	row		services			S1 S2 S3
	#	The population looks like:
	#					A	B	C	D
	#				S1	1	0	0	1
	#				S2	0	1	0	0
	#				S3	0	0	1	0
	parent <- vector()
	count <- 0
	repeat {
		check <- 1
		if(count > size - 1){
			break
		}
		chromosome <- rbinom(row * col, 1, 0.5)
		
		#The if the chromosome is valid
		check <- services_constraint_check(chromosome)
		if(is.logical(check)){
			#If the chromosome is invalid, then fix it
			chromosome <- repair_service_minimum(chromosome)
			check <- 1
		}
		check <- cost_constraint_check(chromosome, cost_limitation)
		if(is.logical(chromosome)){
			#If the chromosome is invalid, then fix it
			chromosome <- repair_cost_maximum(chromosome)
		}

		parent <- rbind(parent, chromosome)
		count <- count + 1
	}
	parent
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
		#set.seed(1)
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
			#set.seed(1)
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
	latency <- vector()
	chromosome <- matrix(chromosome, nrow = 3, ncol = 4)
	frequency <- colSums(frequency_matrix)
	total <- rowSums(chromosome)
	for(iter in 1:length(total)){
		if(total[iter] == 0){
			latency[iter] <- sum((frequency[iter])) * sum(latency_matrix[iter, ] * chromosome[iter, ])
		}
		else{
			latency[iter] <- sum((frequency[iter]) / total[iter]) * sum(latency_matrix[iter, ] * chromosome[iter, ])
		}
	}
	#latency <- sum((frequency / total) * rowSums(latency_matrix * chromosome))
	#latency <- sum(chromosome * latency_matrix * frequency_matrix)
	latency <- sum(latency)
	latency
}

normalize <- function(data){
	normalized_data <- vector()
	for(i in 1:ncol(data)){
		min_value <- min(data[, i])
		max_value <- max(data[, i])
		a <- 1
		b <- 0
		#normalized_data <- cbind(normalized_data, (data[, i] - mean(data[, i])) / sd(data[, i]))
		normalized_data <- cbind(normalized_data, (a + (data[, i] - max_value) * (b - a)) / (max_value - min_value))
	}
	normalized_data
}


services_constraint_check <- function(chromosome){
	chromosome_m <- matrix(chromosome, nrow = 3, ncol = 4)
	if(prod(apply(chromosome_m, 1, services_minimum)) == 0) return(F)
	return(chromosome)
}

services_minimum <- function(chromosome){
	res <- sum(chromosome) > 0
	res
}

#If the minimum service number is not achieved, then 
#randomly choose a point add 1 service deployment
repair_service_minimum <- function(chromosome){
	#temp_pop <- vector()
	chromosome_m <- matrix(chromosome, nrow = 3, ncol = 4)
	row <- nrow(chromosome_m)
	col <- ncol(chromosome_m)
	for(iter in 1:row){
		if(sum(chromosome_m[iter, ]) == 0){
			#set.seed(1)
			cutPoint <- floor(runif(1, 1, col))
			chromosome_m[iter, cutPoint] <- 1
		}
	}
	chromosome <- as.vector(chromosome_m)
	chromosome
}

#Limitation of cost
cost_constraint_check <- function(chromosome, limitation){
	chromosome_m <- matrix(chromosome, nrow = 3, ncol = 4)
	cost <- sum(chromosome_m * cost_matrix)
	if(cost > limitation) return(F)
	return(chromosome)
}
repair_cost_maximum <- function(chromosome){
	chromosome_m <- matrix(chromosome, nrow = 3, ncol = 4)
	row <- nrow(chromosome_m)
	col <- ncol(chromosome_m)
	row_num <- vector()
	candidate_chromosome <- vector()
	for(iter in 1:row){
		if(sum(chromosome_m[iter, ]) > 1){
			row_num <- c(row_num, iter)
		}
	}
	for(j in 1:length(row_num)){
		candidate_chromosome <- c(candidate_chromosome, chromosome_m[row_num[j], ])
	}
	candidate_index <- which(candidate_chromosome %in% 1)
	selected_num <- sample(candidate_index, 1, replace = F)
	candidate_chromosome[selected_num] <- 0
	candidate_chromosome <- matrix(candidate_chromosome, byrow = T, nrow = length(row_num), ncol = 4)
	for(k in 1:length(row_num)){
		chromosome_m[row_num[k], ] <- chromosome_m[row_num[k], ] & candidate_chromosome[k, ]
	}
	return(as.vector(chromosome_m))
}


calculate_mean_cost <- function(unNormalized){
	print(mean(unNormalized[, 1]))
}
