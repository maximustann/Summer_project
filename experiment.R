source("pre.R")
source("algorithm.R")
source("ga.R")
library(GA)
library(mco)
library(nsga2R)
library(lpSolve)
library(foreach)
library(doMC)
registerDoMC(8)

run <- function(matrixSize, cost_limitation, initialisation = F){
	NSGAdata <- vector()
	GAdata <- vector()
	NSGAtime<- vector()
	GAtime<- vector()
	predata(matrixSize)
	max_cost <- unNormalized_cost_fitness(matrix(rep(1, matrixSize * matrixSize), nrow = matrixSize), matrixSize)
	min_cost <- unNormalized_cost_fitness(search_minimum_cost(matrixSize), matrixSize)
	max_latency <- search_greatest_latency(matrixSize)
	min_latency <- 0

	foreach(iter = 1:40) %dopar%{
		cat("Generation:", iter, '\n')
		NSGAdata <- run_algorithm(matrixSize, iter, cost_limitation, max_cost, min_cost, max_latency, min_latency, initialisation)
		GAdata <- run_ga(matrixSize, iter, cost_limitation, max_cost, min_cost, max_latency, min_latency, initialisation)
		
		if(initialisation == T){
			GAtime <- GAdata[1, 3]
			filename_ga <- paste(iter, "_initialisation_ga.csv", sep = "")
			filename_ga_time <- paste(iter, "_initialisation_time_ga.csv", sep = "")
			write.csv(GAdata[, 1:2], filename_ga, quote = F, col.names = T, row.names = F)
			write.csv(GAtime, filename_ga_time, quote = F, row.names = F, col.names = F)

			NSGAtime <- NSGAdata[1, 3]
			filename_nsga <- paste(iter, "_initialisation_nsga.csv", sep = "")
			filename_nsga_time <- paste(iter, "_initialisation_time_nsga.csv", sep = "")
			write.csv(NSGAdata[, 1:2], filename_nsga, quote = F, row.names = F)
			write.csv(NSGAtime, filename_nsga_time, quote = F, row.names = F)
		}
		else{
			GAtime <- GAdata[1, 3]
			filename_ga <- paste(iter, "_ga.csv", sep = "")
			filename_ga_time <- paste(iter, "_time_ga.csv", sep = "")
			write.csv(GAdata[, 1:2], filename_ga, quote = F, col.names = T, row.names = F)
			write.csv(GAtime, filename_ga_time, quote = F, row.names = F, col.names = F)
	
			NSGAtime <- NSGAdata[1, 3]
			filename_nsga <- paste(iter, "_nsga.csv", sep = "")
			filename_nsga_time <- paste(iter, "_time_nsga.csv", sep = "")
			write.csv(NSGAdata[, 1:2], filename_nsga, quote = F, row.names = F)
			write.csv(NSGAtime, filename_nsga_time, quote = F, row.names = F)
		}

	}

	for(iter in 1:40){
		if(initialisation == T){
			filename_nsga <- paste(iter, "_initialisation_nsga.csv", sep = "")
			filename_nsga_time <- paste(iter, "_initialisation_time_nsga.csv", sep = "")
			NSGAdata <- rbind(NSGAdata, read.csv(filename_nsga, header = T, sep = ','))
			NSGAtime <- rbind(NSGAtime, read.csv(filename_nsga_time, header = T, sep = ","))
	
			filename_ga <- paste(iter, "_initialisation_ga.csv", sep = "")
			filename_ga_time <- paste(iter, "_initialisation_time_ga.csv", sep = "")
			GAdata <- rbind(GAdata, read.csv(filename_ga, header = T, sep = ','))
			GAtime <- rbind(GAtime, read.csv(filename_ga_time, header = T, sep = ","))
		}
		else{
			filename_nsga <- paste(iter, "_nsga.csv", sep = "")
			filename_nsga_time <- paste(iter, "_time_nsga.csv", sep = "")
			NSGAdata <- rbind(NSGAdata, read.csv(filename_nsga, header = T, sep = ','))
			NSGAtime <- rbind(NSGAtime, read.csv(filename_nsga_time, header = T, sep = ","))
	
			filename_ga <- paste(iter, "_ga.csv", sep = "")
			filename_ga_time <- paste(iter, "_time_ga.csv", sep = "")
			GAdata <- rbind(GAdata, read.csv(filename_ga, header = T, sep = ','))
			GAtime <- rbind(GAtime, read.csv(filename_ga_time, header = T, sep = ","))
		}
	}

	##lptime <- read.csv("1_time_lp.csv", header = T)
	time_data <- rbind(NSGAtime, GAtime)
	best_nsga <- best_nsga_front(NSGAdata)
	#cat("row for best GA data: ", nrow(best_ga), "\n")
	#cat("row for LP data: ", nrow(lpdata), "\n")
	##lpdata <- read.csv("1_lp.csv", sep = ",", header = T)
	#data <- total_normalised(best_ga, lpdata)[, 1:2]
	data <- rbind(best_nsga[, 1:2], GAdata)
	colnames(data) <- c("costF", "latencyF")
	if(initialisation == T){
		filename <- paste(matrixSize, "_initialisation.csv", sep = "")
		filename_time <- paste(matrixSize, "_initialisation_time.csv", sep="")
	}
	else{
		filename <- paste(matrixSize, ".csv", sep = "")
		filename_time <- paste(matrixSize, "_time.csv", sep="")
	}
	write.csv(data, filename, quote = F, row.names = F)
	write.csv(time_data, filename_time, quote = F, row.names = F)
}

best_nsga_front <- function(gadata){
	temp <- unique(gadata[, 1:2])
	ga <- vector()
	best_ga <- vector()
	ranking <- fastNonDominatedSorting(temp)
	rnkIndex <- integer()
	i <- 1
	while (i <= length(ranking)){
		rnkIndex[ranking[[i]]] <- i
		i <- i + 1
	}
	ga <- cbind(temp, rnkIndex)
	best_ga <- ga[ga[, 3] == 1,]
	print(best_ga)
	best_ga
}

total_normalised <- function(gadata, lpdata){
	#print(lpdata)
	data <- rbind(gadata[, 1:2], lpdata)
	normalized_data <- vector()
	for(i in 1:ncol(data)){
		min_value <- min(data[, i])
		max_value <- max(data[, i])
		a <- 0
		b <- 1
		normalized_data <- cbind(normalized_data, a + ((data[, i] - min_value) * (b - a) / (max_value - min_value)))
	}
	normalized_data
}

#euclidean_distance <- function(vect, weight_for_cost = 0.5){
	#dist <- sqrt((weight_for_cost * vect[1])^2 + ((1 - weight_for_cost) * vect[2])^2)
	#dist
#}

search_maximum_latency <- function(matrixSize){
	initial_matrix <- matrix(c(rep(1, matrixSize), rep(0, matrixSize * (matrixSize - 1))), nrow = matrixSize)
	#initial_matrix <- as.integer(initial_matrix)
	#print(initial_matrix)
	latency <- 0.0
	for(row_iter in 1:matrixSize){
		for(col_iter in 1:matrixSize){
			if(row_iter == 1 && col_iter == 1){
				latency <- unNormalized_latency_fitness(initial_matrix, matrixSize)
				next
			}
			num <- which(initial_matrix[row_iter, ] == 1)
			initial_matrix[row_iter, num] <- 0
			initial_matrix[row_iter, col_iter] <- 1
			current_latency <- unNormalized_latency_fitness(initial_matrix, matrixSize)
			if(current_latency > latency){
				latency <- current_latency
			}
			else{
				initial_matrix[row_iter, col_iter] <- 0
				initial_matrix[row_iter, num] <- 1
			}
		}
	}
	latency
}

search_minimum_cost <- function(matrixSize){
	mini_cost_matrix <- matrix(rep(0, matrixSize * matrixSize), nrow = matrixSize)
	for(row_iter in 1:matrixSize){
		pos <- which(cost_matrix[row_iter, ] == min(cost_matrix[row_iter, ]))[1]
		mini_cost_matrix[row_iter, pos] <- 1
	}
	#print(mini_cost_matrix)
	mini_cost_matrix
}
