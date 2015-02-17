library(foreach)
library(GA)
library(mco)
library(nsga2R)
library(lpSolve)

run_greedy <- function(matrixSize, cost_limitation){
	predata(matrixSize)
	ptm <- proc.time()
	solution <- drop(cost_limitation, matrixSize)
	print(proc.time() - ptm)
	result <- evaluate_solution(solution, matrixSize)
	result <- c(result, (proc.time() - ptm)[1])
	names(result) <- c("costF", "latencyF", "time")
	result
}

#latency_fitness <- function(chromosome, matrixSize){
	#latency <- 0.0
	#chromosome <- matrix(chromosome, nrow = matrixSize, ncol = matrixSize)
	#frequency <- colSums(frequency_matrix)
	#num_of_Usercenter <- matrixSize
	#for(service_iter in 1:matrixSize){
		#num_of_service <- sum(chromosome[service_iter, ])
		#deployed_service <- which(chromosome[service_iter, ] != 0)
		#locationLatency <- 0.0
		#for(latency_iter in 1:matrixSize){
			#if(0 %in% latency_matrix[latency_iter, ]){
				#if(chromosome[service_iter, which(latency_matrix[latency_iter, ] == 0)] == 1){
					#locationLatency <- locationLatency + 0.0
				#}
				#else{
					#locationLatency <- locationLatency + frequency[service_iter] / (num_of_service * matrixSize) * sum(latency_matrix[latency_iter, deployed_service])
				#}
			#}
			#else{
				#locationLatency <- locationLatency + frequency[service_iter] / (num_of_service * matrixSize) * sum(latency_matrix[latency_iter, deployed_service])
			#}
		#}
		#latency <- latency + locationLatency
	#}
	#latency
#}

#cost_fitness <- function(chromosome, matrixSize){
	#chromosome <- matrix(chromosome, nrow = matrixSize, ncol = matrixSize)
	#cost <- sum(chromosome * cost_matrix)
	#cost
#}
drop <- function(cost_limitation = 300, matrixSize){
	chromosome <- rep(1, matrixSize * matrixSize) * cost_matrix
	initial_solution <- chromosome
	assign.costs <- chromosome
	lp_time <- proc.time()
	lp.assign(assign.costs)
	cat("lp time: ", proc.time() - lp_time, "\n", sep = "")
	initial_solution <- lp.assign(assign.costs)$solution
	solution <- ala(initial_solution, cost_limitation, matrixSize)
	solution
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
			if(currentF > latencyF && cost_fitness(current_solution, matrixSize) <= cost_limitation){
				solution <- current_solution
			}
		}
	}
	solution
}

evaluate_solution <- function(solution, matrixSize, weight_for_cost = 0.5){
	print(solution)
	cost_fit <- cost_fitness(solution, matrixSize)
	latency_fit <- latency_fitness(solution, matrixSize)
	result <- c(cost_fit, latency_fit)
	result
}
