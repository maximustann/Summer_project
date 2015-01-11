source("pre.R")
library(lpSolve)


return_row_col <- function(chromosome){
	nRow <- nrow(chromosome)
	nCol <- ncol(chromosome)
	c(nRow, nCol)
}

latency_fitness <- function(chromosome){
	row_col <- return_row_col(chromosome)
	latency <- vector()
	chromosome <- matrix(chromosome, nrow = row_col[1], ncol = row_col[2])
	frequency <- colSums(frequency_matrix)
	total <- rowSums(chromosome)
	for(iter in 1:length(total)){
		latency[iter] <- sum(frequency[iter] / total[iter]) * sum(latency_matrix[iter, ] * chromosome[iter, ])
	}
	latency <- sum(latency)
	latency
}

cost_fitness <- function(chromosome){
	row_col <- return_row_col(chromosome)
	chromosome <- matrix(chromosome, nrow = row_col[1], ncol = row_col[2])
	cost <- sum(chromosome * cost_matrix)
	cost
}
drop <- function(cost_limitation = 1000){
	row_col <- return_row_col(cost_matrix)
	chromosome <- rep(1, row_col[1] * row_col[2]) * cost_matrix
	initial_solution <- chromosome
	assign.costs <- chromosome
	lp.assign(assign.costs)
	initial_solution <- lp.assign(assign.costs)$solution
	print(initial_solution)
	ala(initial_solution, cost_limitation)
}

ala <- function(initial_solution, cost_limitation){
	row_col <- return_row_col(initial_solution)
	solution <- initial_solution
	for(iter in 1:row_col[1]){
		for(citer in 1:row_col[2]){
			latencyF <- latency_fitness(solution)
			temp_solution <- solution
			temp_solution[iter, citer] <- 1
			current_solution <- temp_solution
			#test if it is better than previous solution
			currentF <- latency_fitness(current_solution)
			print(cost_fitness(current_solution))
			if(currentF > latencyF && cost_fitness(current_solution) <= cost_limitation){
				solution <- current_solution
			}
		}
	}
	print("Now is the solution:")
	print(cost_fitness(solution))
	print(latency_fitness(solution))
	solution
}
