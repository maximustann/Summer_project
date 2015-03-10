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
	colnames(unNormalized) <- c("costF", "latencyF", "time")
	unNormalized
}



algorithm <- function(matrixSize, seed, cost_limitation){

	popSize <- 100
	objDim <- 2
	varNo <- matrixSize * matrixSize
	tourSize <- 10
	MutDistIdx <- 20
	mprob <- 0.2
	XoverDistIdx <- 20
	cprob <- 0.8
	generations <- 100
	#cost_limitation <- 100000
	front <- vector()
	front_pool <- vector()
#=============================================================================
set.seed(seed)
#========================Algorithm Starts=====================================
	#	step 1, initialize the population
	parent <- generate_population(matrixSize, matrixSize, popSize, cost_limitation, matrixSize)

#====================Normalized Fitness================================
	#	step 2, calculate the fitness
	unNormalized <- t(apply(parent, 1, fitness, matrixSize = matrixSize))
	parent <- cbind(parent, unNormalized)
#====================Normalization Ends================================

	#	step 3, rank it
	ranking <- fastNonDominatedSorting(parent[, (varNo + 1):(varNo + objDim)])
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
		childAfterX <- crossover(matingPool[, 1:varNo], cprob)
	
		#	step 7 Mutation
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
		
		#check existed
		childAfterMutation <- check_existed(childAfterM, front_pool, varNo)
	tmp_time <- proc.time()
		unNormalized <- t(apply(childAfterMutation[[1]], 1, fitness, matrixSize))
	repair_time <- repair_time + ((proc.time() - tmp_time)[1])
		#Normalized data
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
		cd <- crowdingDist4frnt(parentNext, ranking, objRange)
		parentNext <- cbind(parentNext, apply(cd, 1, sum))
		parentNext.sort <- parentNext[order(parentNext[, varNo + objDim + 1]), ]
		parent <- parentNext.sort[1:popSize, ]
		front <- parent[parent[, varNo + 3] == 1, ]
		if (is.matrix(front) == F){
			front <- matrix(front, ncol = varNo + 4)
		}
		#update front_pool
		front_pool <- front
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
	#cat("Repair time: ", repair_time, "\n", sep = "")
	unique(front[, 1:varNo])
}



check_existed <- function(childAfterM, front_pool, varNo){
	child_existed <- vector()
	child_non_existed <- vector()
	num_in_children <- nrow(childAfterM)
	front_pool <- matrix(front_pool, nrow = 1)
	num_in_front <- nrow(front_pool)
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
	
	unNormalized <- t(apply(front[, 1:(matrixSize * matrixSize)], 1, fitness, matrixSize))
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

cost_fitness <- function(chromosome, matrixSize){
	chromosome <- matrix(chromosome, nrow = matrixSize, ncol = matrixSize)
	cost <- sum(chromosome * cost_matrix)
	cost
}

latency_fitness <- function(chromosome, matrixSize){

	chromosome <- as.integer(chromosome)
	chromosome <- matrix(chromosome, nrow = matrixSize, ncol = matrixSize)
	latency <- 0.0
	frequency <- colSums(frequency_matrix)
	num_of_Usercenter <- matrixSize
	response_matrix <- vector()
	for(user_iter in 1:matrixSize){
		for(service_iter in 1:matrixSize){
			num_of_service <- sum(chromosome[service_iter, ])
			deployed_service <- which(chromosome[service_iter, ] == 1)
			if(num_of_service > 1){
				response_matrix <- c(response_matrix, min(latency_matrix[service_iter, deployed_service]))
			}
			else{
				response_matrix <- c(response_matrix, latency_matrix[service_iter, deployed_service])
			}
		}
	}
	response_matrix <- matrix(response_matrix, nrow = matrixSize, matrixSize, byrow = T)
	latency <- sum(response_matrix * frequency_matrix)

	latency
}

normalize <- function(data, max_value, min_value){
	normalized_data <- vector()
	for(i in 1:ncol(data)){
		#min_value <- min(data[, i])
		#max_value <- max(data[, i])
		a <- 0
		b <- 1
		normalized_data <- cbind(normalized_data, ((data[, i] - min_value) * (b - a) / (max_value - min_value)))
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
