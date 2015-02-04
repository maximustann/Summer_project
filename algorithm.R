library(foreach)
library(GA)
library(mco)
library(nsga2R)
run_algorithm <- function(matrixSize, seed = 1, cost_limitation){
	
	predata(matrixSize)
	print(cost_matrix)
	print(latency_matrix)
	print(frequency_matrix)
	ptm <- proc.time()
	front <- algorithm(matrixSize, seed, cost_limitation)
	print((proc.time() - ptm)[1])
	unNormalized <- evaluate_front(front, matrixSize)
	unNormalized <- cbind(unNormalized, (proc.time() - ptm)[1])
	#print(unNormalized)
	colnames(unNormalized) <- c("costF", "latencyF", "time")
	#filename <- paste(seed, ".csv", sep = "")
	#write.csv(unNormalized, filename, quote = F, row.names = F)
	unNormalized
}



algorithm <- function(matrixSize, seed, cost_limitation){
	#	The population size is 100
	#	We only have two objectives, so objDim = 2
	#	varNo is the variable Number, which is matrixSize * matrixSize
	#	Mutation distribution index, don't know how it works yet*
	#	Crossover distribution index, don't know how it works either*
	#	cprob is Crossover probability
	#	mprob is Mutation probability
#======================initialize parameters==================================
	#candidate_cities_num <- length(candidate_cities)
	#services_num <- length(services)
	#candidate_cities_num <- matrixSize
	#services_num <- matrixSize

	popSize <- 50
	objDim <- 2
	varNo <- matrixSize * matrixSize
	tourSize <- 10
	MutDistIdx <- 20
	mprob <- 0.2
	XoverDistIdx <- 20
	cprob <- 0.8
	generations <- 50
	#cost_limitation <- 100000
	front <- vector()
	front_pool <- vector()
#=============================================================================
set.seed(seed)
#========================Algorithm Starts=====================================
	#	step 1, initialize the population
	parent <- generate_population(matrixSize, matrixSize, popSize, cost_limitation, matrixSize)
	#print(parent)

#====================Normalized Fitness================================
	#	step 2, calculate the fitness
	unNormalized <- t(apply(parent, 1, fitness, matrixSize = matrixSize))
	#calculate_mean_cost(unNormalized)
	#normalized the data
	#parent <- cbind(parent, normalize(unNormalized))
	#Do not Normalize the data
	parent <- cbind(parent, unNormalized)
	#pop <- cbind(pop, t(apply(pop, 1, fitness)))
	#origin_range <- range(c(min(pop[, varNo + 1]), max(pop[, varNo + 1])))
#====================Normalization Ends================================

	#origin_range_y <- range(0, 1)
	#origin_range_x <- range(0, 1)
	#plot(parent[, (varNo + 1):(varNo + objDim)], xlim = origin_range_x, ylim = origin_range_y, col = 'blue', xlab = 'cost', ylab = 'latency')


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

	parent <- cbind(parent, apply(cd, 1, sum))
	#initialize the front_pool
	front_pool <- parent[parent[, varNo + 3] == 1, ]

	repair_time <- 0.0
	for(iter in 1:generations){
		#step 5, selection
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
			chromosome <- services_constraint_check(childAfterM[j, ], matrixSize)
			if(is.logical(chromosome)){
				childAfterM[j, ] <- repair_service_minimum(childAfterM[j, ], matrixSize)
			}
			chromosome <- cost_constraint_check(childAfterM[j, ], cost_limitation, matrixSize)
			if(is.logical(chromosome)){
				childAfterM[j, ] <- repair_cost_maximum(childAfterM[j, ], cost_limitation, matrixSize)
			}
		}

		#print(childAfterM)
		#childAfterM <- cbind(childAfterM, t(apply(childAfterM, 1, fitness)))
		
		#check existed
		childAfterMutation <- check_existed(childAfterM, front_pool, varNo)
	tmp_time <- proc.time()
		unNormalized <- t(apply(childAfterMutation[[1]], 1, fitness, matrixSize))
	repair_time <- repair_time + ((proc.time() - tmp_time)[1])
		#print(childAfterMutation[[2]])
		#calculate_mean_cost(unNormalized)
		#Normalized data
		#childAfterMutation[[1]] <- cbind(childAfterMutation[[1]], normalize(unNormalized))
		childAfterMutation[[1]] <- cbind(childAfterMutation[[1]], unNormalized)
		childAfterM <- rbind(childAfterMutation[[1]], childAfterMutation[[2]])
		parentNext <- rbind(parent[, 1:(varNo + objDim)], childAfterM)
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
		#row * col + cost_fitness + network_latency_fitness, then next one is ranking
		front <- parent[parent[, varNo + 3] == 1, ]
		if (is.matrix(front) == F){
			#the number of variable + 2 fitnesses + ranking + crowdingDistance
			front <- matrix(front, ncol = varNo + 4)
		}
		#update front_pool
		front_pool <- front
		#print("it's front pool")
		#front_pool <- rbind(front_pool, check_existed_front(front, front_pool, varNo))
		#print(apply(front[, 1:varNo], 1, fitness))
		#print(front)
		#par(new = T)
		#plot(front[, (varNo + 1):(varNo + objDim)], xlim = origin_range_x, ylim = origin_range_y, col = 'red', pch = 4)
	}
	#par(new = T)
	#plot(parent[, (varNo + 1):(varNo + objDim)], xlim = origin_range_x, ylim = origin_range_y, col = 'red', pch = 3)
	#plot(front[, (varNo + 1):(varNo + objDim)], xlim = origin_range_x, ylim = origin_range_y, bg = 'black', pch = 24)

	result = list(functions = fitness, parameterDim = varNo, objectiveDim = objDim, popSize = popSize,
				  tournamentSize = tourSize, generations = generations, XoverProb = cprob, mutationProb = mprob,
				  parameters = parent[,1:varNo], objectives = parent[, (varNo + 1):(varNo + objDim)], 
				  paretoFrontRank = parent[, varNo + objDim + 1], crowdingDistance = parent[, varNo + objDim + 2])
	class(result) = "nsga2R"
	#print(unique(front))
	cat("Repair time: ", repair_time, "\n", sep = "")
	unique(front[, 1:varNo])
}



check_existed <- function(childAfterM, front_pool, varNo){
	child_existed <- vector()
	child_non_existed <- vector()
	num_in_children <- nrow(childAfterM)
	num_in_front <- nrow(front_pool)
	#print(num_in_front)
	if(num_in_front == 0){
		child_non_existed <- childAfterM
		child_existed <- NULL
		return(list(child_non_existed, child_existed))
	}
	for(i in 1:num_in_children){
		for(j in 1:num_in_front){
			if(identical(childAfterM[i, ], front_pool[j, 1:varNo])) {
				child_existed <- rbind(child_existed, front_pool[j, 1:(varNo + 2)])
				break
			}
			if(j == num_in_front){
				child_non_existed <- rbind(childAfterM[i, ], child_non_existed)
			}
		}
	}
	list(child_non_existed, child_existed)
}

evaluate_front <- function(front, matrixSize){
	
	#for(iter in 1:nrow(front)){
		#print(matrix(front[iter, 1:(matrixSize * matrixSize)], nrow = matrixSize, ncol = matrixSize))
	#}
	unNormalized <- t(apply(front[, 1:(matrixSize * matrixSize)], 1, fitness, matrixSize))
	#dist <- apply(unNormalized, 1, euclidean_distance)
	#unNormalized <- cbind(unNormalized, dist)
	unNormalized
}



generate_population <- function(row, col, size, cost_limitation, matrixSize){
	parent <- vector()
	count <- 0
	repeat {
		check <- 1
		if(count > size - 1){
			break
		}
		chromosome <- rbinom(row * col, 1, 0.5)
		
		#Check the chromosome is valid
		check <- services_constraint_check(chromosome, matrixSize)
		if(is.logical(check)){
			#If the chromosome is invalid, then fix it
			chromosome <- repair_service_minimum(chromosome, matrixSize)
			check <- 1
		}
		check <- cost_constraint_check(chromosome, cost_limitation, matrixSize)
		if(is.logical(chromosome)){
			#If the chromosome is invalid, then fix it
			chromosome <- repair_cost_maximum(chromosome, cost_limitation, matrixSize)
		}
		parent <- rbind(parent, chromosome)
		count <- count + 1
	}
	parent
}

fitness <- function(chromosome, matrixSize) {
	cost <- cost_fitness(chromosome, matrixSize)
	latency <- latency_fitness(chromosome, matrixSize)
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

cost_fitness <- function(chromosome, matrixSize){
	chromosome <- matrix(chromosome, nrow = matrixSize, ncol = matrixSize)
	cost <- sum(chromosome * cost_matrix)
	cost
}

latency_fitness <- function(chromosome, matrixSize){
	#latency <- vector()
	chromosome <- matrix(chromosome, nrow = matrixSize, ncol = matrixSize)
	latency <- 0.0
	frequency <- colSums(frequency_matrix)
	num_of_Usercenter <- matrixSize

	for(service_iter in 1:matrixSize){
		num_of_service <- sum(chromosome[service_iter, ])
		deployed_service <- which(chromosome[service_iter, ] != 0)
		#cat("which(chromosome[service_iter, ] == 1)", which(chromosome[service_iter, ] == 1), '\n')
		locationLatency <- 0.0
		#the matrixSize here represent the number of user center
		for(latency_iter in 1:matrixSize){
			if(0 %in% latency_matrix[latency_iter, ]){
				if(chromosome[service_iter, which(latency_matrix[latency_iter, ] == 0)] == 1){
					locationLatency <- locationLatency + 0.0
				}
				else{
					locationLatency <- locationLatency + frequency[service_iter] / (num_of_service * matrixSize) * sum(latency_matrix[latency_iter, deployed_service])
				}
			}
			else{
				locationLatency <- locationLatency + frequency[service_iter] / (num_of_service * matrixSize) * sum(latency_matrix[latency_iter, deployed_service])
			}
		}
		latency <- latency + locationLatency
	}

	latency

}

normalize <- function(data){
	normalized_data <- vector()
	for(i in 1:ncol(data)){
		min_value <- min(data[, i])
		max_value <- max(data[, i])
		a <- 0
		b <- 1
		#normalized_data <- cbind(normalized_data, (data[, i] - mean(data[, i])) / sd(data[, i]))
		normalized_data <- cbind(normalized_data, a + ((data[, i] - min_value) * (b - a) / (max_value - min_value)))
	}
	normalized_data
}


services_constraint_check <- function(chromosome, matrixSize){
	chromosome_m <- matrix(chromosome, nrow = matrixSize, ncol = matrixSize)
	if(prod(apply(chromosome_m, 1, services_minimum)) == 0) return(F)
	return(chromosome)
}

services_minimum <- function(chromosome){
	res <- sum(chromosome) > 0
	res
}

#If the minimum service number is not achieved, then 
#randomly choose a point add 1 service deployment
repair_service_minimum <- function(chromosome, matrixSize){
	chromosome_m <- matrix(chromosome, nrow = matrixSize, ncol = matrixSize)
	for(iter in 1:matrixSize){
		if(sum(chromosome_m[iter, ]) == 0){
			cutPoint <- floor(runif(1, 1, matrixSize))
			chromosome_m[iter, cutPoint] <- 1
		}
	}
	chromosome <- as.vector(chromosome_m)
	chromosome
}

#Limitation of cost
cost_constraint_check <- function(chromosome, limitation, matrixSize){
	chromosome_m <- matrix(chromosome, nrow = matrixSize, ncol = matrixSize)
	cost <- sum(chromosome_m * cost_matrix)
	if(cost > limitation) return(F)
	return(chromosome)
}
repair_cost_maximum <- function(chromosome, limitation, matrixSize){
	chromosome_m <- matrix(chromosome, nrow = matrixSize, ncol = matrixSize)
	repeat {
		row_num <- vector()
		#firstly, we check which row has more than one service deployed
		for(iter in 1:matrixSize){
			if(sum(chromosome_m[iter, ]) > 1){
				row_num <- c(row_num, iter)
			}
		}
		#Secondly, we check if there is no more improvement we can make
		if(length(row_num) == 0){
			return(as.vector(chromosome_m))
		}

		#Thirdly, find the multiple deployment, vectorized them and then randomly select one of them to drop
		#After that, do AND operation with the original chromosome so that it will change the original one
		candidate_chromosome <- chromosome_m[row_num, ]
		candidate_chromosome <- as.vector(candidate_chromosome)
		candidate_index <- which(candidate_chromosome %in% 1)
		selected_num <- sample(candidate_index, 1, replace = F)
		candidate_chromosome[selected_num] <- 0
		chromosome_m[row_num, ] <- chromosome_m[row_num, ] & candidate_chromosome
		
		chromosome <- as.vector(chromosome_m)
		check <- cost_constraint_check(chromosome, limitation, matrixSize)
		if(is.logical(check) != T) {
			return(chromosome)
		}
	}
}

calculate_mean_cost <- function(unNormalized){
	print(mean(unNormalized[, 1]))
}
