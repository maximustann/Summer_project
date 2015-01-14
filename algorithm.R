source("pre.R")
library(foreach)
library(GA)
library(mco)
library(nsga2R)

run_algorithm <- function(matrixSize){
	predata(matrixSize)
	print(cost_matrix)
	print(latency_matrix)
	print(frequency_matrix)
	ptm <- proc.time()
	front <- algorithm(matrixSize)
	print(proc.time() - ptm)
	evaluate_front(front, matrixSize)
}

algorithm <- function(matrixSize){
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
	cost_limitation <- 300
	front <- vector()
	front_pool <- vector()
#=============================================================================
set.seed(NULL)
#========================Algorithm Starts=====================================
	#	step 1, initialize the population
	parent <- generate_population(matrixSize, matrixSize, popSize, cost_limitation, matrixSize)
	#print(parent)

#====================Normalized Fitness================================
	#	step 2, calculate the fitness
	unNormalized <- t(apply(parent, 1, fitness, matrixSize = matrixSize))
	#calculate_mean_cost(unNormalized)
	parent <- cbind(parent, normalize(unNormalized))
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

	#	step 5, selection
	parent <- cbind(parent, apply(cd, 1, sum))
	#initialize the front_pool
	front_pool <- parent[parent[, varNo + 3] == 1, ]

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
			chromosome <- services_constraint_check(childAfterM[j, ], matrixSize)
			if(is.logical(chromosome)){
				childAfterM[j, ] <- repair_service_minimum(childAfterM[j, ], matrixSize)
			}
			chromosome <- cost_constraint_check(childAfterM[j, ], cost_limitation, matrixSize)
			if(is.logical(chromosome)){
				#print(matrix(childAfterM[j, ], nrow = 3, ncol = 4))
				childAfterM[j, ] <- repair_cost_maximum(childAfterM[j, ], cost_limitation, matrixSize)
				#print(matrix(childAfterM[j, ], nrow = 3, ncol = 4))
			}
		}

		#print(childAfterM)
		#childAfterM <- cbind(childAfterM, t(apply(childAfterM, 1, fitness)))
		
		#check existed
		childAfterMutation <- check_existed(childAfterM, front_pool, varNo)
		#print(mode(childAfterMutation[[1]]))
		unNormalized <- t(apply(childAfterMutation[[1]], 1, fitness, matrixSize))
		#calculate_mean_cost(unNormalized)
		childAfterMutation[[1]] <- cbind(childAfterMutation[[1]], normalize(unNormalized))
		#print(childAfterMutation[[2]])
		childAfterM <- rbind(childAfterMutation[[1]], childAfterMutation[[2]])
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
		#row * col + cost_fitness + network_latency_fitness, then next one is ranking
		front <- parent[parent[, varNo + 3] == 1, ]
		#update front_pool
		front_pool <- front
		#print("it's front pool")
		#front_pool <- rbind(front_pool, check_existed_front(front, front_pool, varNo))
		#print(apply(front[, 1:varNo], 1, fitness))
		#par(new = T)
		#plot(front[, (varNo + 1):(varNo + objDim)], xlim = origin_range_x, ylim = origin_range_y, col = 'red', pch = 4)
	}
	#par(new = T)
	#plot(parent[, (varNo + 1):(varNo + objDim)], xlim = origin_range_x, ylim = origin_range_y, col = 'red', pch = 3)
	#plot(front[, (varNo + 1):(varNo + objDim)], xlim = origin_range_x, ylim = origin_range_y, bg = 'black', pch = 24)
	#dev.off()
	#pdf("plot.pdf")

	result = list(functions = fitness, parameterDim = varNo, objectiveDim = objDim, popSize = popSize,
				  tournamentSize = tourSize, generations = generations, XoverProb = cprob, mutationProb = mprob,
				  parameters = parent[,1:varNo], objectives = parent[, (varNo + 1):(varNo + objDim)], 
				  paretoFrontRank = parent[, varNo + objDim + 1], crowdingDistance = parent[, varNo + objDim + 2])
	class(result) = "nsga2R"
	#for(iter in 1:nrow(front)){
		#print(matrix(front[iter, 1:varNo], nrow = matrixSize, ncol = matrixSize))
	#}
	#print(front)
	#print(front[, 1:(row_col[1] * row_col[2])])
	#print("The number of the candidate in the front")
	#print(nrow(front))
	#unNormalized <- t(apply(front[, 1:(row_col[1] * row_col[2])], 1, fitness))
	#print(unNormalized)
	#return(parent)
	#return(paretoFront(result))
	#return(result)
	front
}

check_existed_front <- function(front, front_pool, varNo){
	front_non_existed <- vector()
	num_in_front <- nrow(front)
	num_in_front_pool <- nrow(front_pool)
	for(i in 1:num_in_front){
		for(j in 1:num_in_front_pool){
			if(identical(front[i, 1:varNo], front_pool[j, 1:varNo]) == F){
				front_non_existed <- rbind(front_non_existed, front[i, ])
			}
		}
	}
	front_non_existed
}





check_existed <- function(childAfterM, front_pool, varNo){
	child_existed <- vector()
	child_non_existed <- vector()
	num_in_children <- nrow(childAfterM)
	num_in_front <- nrow(front_pool)
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
	print(unNormalized)
}


generate_population <- function(row, col, size, cost_limitation, matrixSize){
	#	population is a list, every index contains a matrix, which is a chromosome
	#	col		candidate_cities:	A B C D
	#	row		services			S1 S2 S3
	#	The population looks like:
	#					A	B	C	D
	#				S1	1	0	0	1
	#				S2	0	1	0	0
	#				S3	0	0	1	0
	#				S4	0	0	1	0
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
	#frequency <- colSums(frequency_matrix)
	#total <- rowSums(chromosome)
	#for(iter in 1:length(total)){
		#latency[iter] <- sum(frequency[iter] / total[iter]) * sum(latency_matrix[iter, ] * chromosome[iter, ])
	#}
	##latency <- sum((frequency / total) * rowSums(latency_matrix * chromosome))
	##latency <- sum(chromosome * latency_matrix * frequency_matrix)
	latency <- 0.0
	frequency <- colSums(frequency_matrix)
	num_of_Usercenter <- matrixSize
	for(service_iter in 1:matrixSize){
		num_of_service <- sum(chromosome[service_iter, ])
		deployed_service <- which(chromosome[service_iter, ] == 1)
		latency <- latency + (frequency[service_iter] / (num_of_service * matrixSize)) * sum(latency_matrix[, deployed_service])
		
	}
	#total <- rowSums(chromosome)
	#for(iter in 1:length(total)){
		#latency[iter] <- sum(frequency[iter] / total[iter]) * sum(latency_matrix[iter, ] * chromosome[iter, ])
	#}
	#latency <- sum(latency)
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
	#temp_pop <- vector()
	chromosome_m <- matrix(chromosome, nrow = matrixSize, ncol = matrixSize)
	for(iter in 1:matrixSize){
		if(sum(chromosome_m[iter, ]) == 0){
			#set.seed(1)
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
	#repeat {
		#row_num <- vector()
		#candidate_chromosome <- vector()
		#for(iter in 1:row){
			#if(sum(chromosome_m[iter, ]) > 1){
				#row_num <- c(row_num, iter)
			#}
		#}
		#if(length(row_num) == 1 && row_num == 0){
			#return(as.vector(chromosome_m))
		#}
		#for(j in 1:length(row_num)){
			#candidate_chromosome <- c(candidate_chromosome, chromosome_m[row_num[j], ])
		#}
		#candidate_index <- which(candidate_chromosome %in% 1)
		#selected_num <- sample(candidate_index, 1, replace = F)
		#candidate_chromosome[selected_num] <- 0
		##print(length(row_num))
		#candidate_chromosome <- matrix(candidate_chromosome, byrow = T, nrow = length(row_num), ncol = 4)
		#for(k in 1:length(row_num)){
			#chromosome_m[row_num[k], ] <- chromosome_m[row_num[k], ] & candidate_chromosome[k, ]
		#}
		#chromosome <- as.vector(chromosome_m)
		#check <- cost_constraint_check(chromosome, limitation)
		#if(is.logical(check) != T){
			#return(chromosome)
		#}
	#}
}

calculate_mean_cost <- function(unNormalized){
	print(mean(unNormalized[, 1]))
}
