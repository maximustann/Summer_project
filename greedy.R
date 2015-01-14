source("pre.R")
library(lpSolve)

run_greedy <- function(matrixSize){
	print(cost_matrix)
	print(latency_matrix)
	print(frequency_matrix)
	predata(matrixSize)
	ptm <- proc.time()
	drop(300, matrixSize)
	print(proc.time() - ptm)
}

latency_fitness <- function(chromosome, matrixSize){
	latency <- 0.0
	#chromosome <- matrix(chromosome, nrow = matrixSize, ncol = matrixSize)
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

cost_fitness <- function(chromosome, matrixSize){
	chromosome <- matrix(chromosome, nrow = matrixSize, ncol = matrixSize)
	cost <- sum(chromosome * cost_matrix)
	cost
}
drop <- function(cost_limitation = 300, matrixSize){
	chromosome <- rep(1, matrixSize * matrixSize) * cost_matrix
	initial_solution <- chromosome
	assign.costs <- chromosome
	lp.assign(assign.costs)
	initial_solution <- lp.assign(assign.costs)$solution
	#print(initial_solution)
	ala(initial_solution, cost_limitation, matrixSize)
}

ala <- function(initial_solution, cost_limitation, matrixSize){
	solution <- initial_solution
	for(iter in 1:matrixSize){
		for(citer in 1:matrixSize){
			latencyF <- latency_fitness(solution, matrixSize)
			temp_solution <- solution
			temp_solution[iter, citer] <- 1
			current_solution <- temp_solution
			#test if it is better than previous solution
			currentF <- latency_fitness(current_solution, matrixSize)
			#print(cost_fitness(current_solution))
			if(currentF > latencyF && cost_fitness(current_solution, matrixSize) <= cost_limitation){
				solution <- current_solution
			}
		}
	}
	print("Now is the solution:")
	print(solution)
	print(cost_fitness(solution, matrixSize))
	print(latency_fitness(solution, matrixSize))
	solution
}
