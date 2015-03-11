library(foreach)
library(GA)
library(mco)
library(nsga2R)
library(lpSolve)

run_greedy <- function(matrixSize, seed, cost_limitation){
	print(seed)
	predata(matrixSize)
	ptm <- proc.time()
	solution <- drop(cost_limitation, matrixSize, seed)
	#print(proc.time() - ptm)
	result <- evaluate_solution(solution, matrixSize)
	result <- c(result, (proc.time() - ptm)[1])
	#names(result) <- c("costF", "latencyF", "time")
	result
}


drop <- function(cost_limitation = 300, matrixSize, seed){
	chromosome <- rep(1, matrixSize * matrixSize) * cost_matrix
	assign.costs <- chromosome
	lp_time <- proc.time()
	lp.assign(assign.costs)
	#cat("lp time: ", proc.time() - lp_time, "\n", sep = "")
	initial_solution <- lp.assign(assign.costs)$solution
	print(initial_solution)
	solution <- ala(initial_solution, cost_limitation, matrixSize, seed)
	solution
}

ala <- function(initial_solution, cost_limitation, matrixSize, seed){
	set.seed(seed)
	solution <- initial_solution
	candidate_chromosome <- as.integer(as.vector(solution))

	for(iter in 1:matrixSize){
		if(iter == 1){
			latencyF <- latency_fitness(solution, matrixSize)
		}
		candidate_index <- which(candidate_chromosome %in% 0)
		selected_num <- sample(candidate_index, 1, replace = F)

		candidate_chromosome[selected_num] <- 1
		#test if it is better than previous solution
		currentF <- latency_fitness(matrix(candidate_chromosome, nrow = matrixSize), matrixSize)
		if(currentF < latencyF && cost_fitness(matrix(candidate_chromosome, nrow = matrixSize), matrixSize) <= cost_limitation){
			#doing nothing
		}
		else{
			candidate_chromosome[selected_num] <- 0
		}
	}
	return(matrix(candidate_chromosome, nrow = matrixSize))
}

evaluate_solution <- function(solution, matrixSize, weight_for_cost = 0.5){
	print(solution)
	cost_fit <- cost_fitness(solution, matrixSize)
	latency_fit <- latency_fitness(solution, matrixSize)
	result <- c(cost_fit, latency_fit)
	result
}
