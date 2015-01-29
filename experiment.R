source("pre.R")
source("algorithm.R")
source("greedy.R")
library(GA)
library(mco)
library(nsga2R)
library(lpSolve)
library(foreach)
#library(doMC)
#registerDoMC(8)

run <- function(matrixSize, cost_limitation){
	gadata <- vector()
	for(iter in 1:40) {
		cat("Generation:", iter, '\n')
		gadata <- rbind(gadata, run_algorithm(matrixSize, iter, cost_limitation))
		lpdata <- run_greedy(matrixSize, cost_limitation)
		#data <- total_normalised(gadata, lpdata)
		#dist <- apply(data, 1, euclidean_distance)
		#data <- cbind(data, dist)
		#colnames(data) <- c("costF", "latencyF", "time")
		#filename <- paste(iter, ".csv", sep = "")
		#write.csv(data, filename, quote = F, row.names = F)
	}
	best_ga <- best_ga_front(gadata)
	data <- total_normalised(best_ga, lpdata)[, 1:2]
	colnames(data) <- c("costF", "latencyF")
	filename <- paste(matrixSize, ".csv", sep = "")
	write.csv(data, filename, quote = F, row.names = F)
}

best_ga_front <- function(gadata){
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
	best_ga
}

total_normalised <- function(gadata, lpdata){
	data <- rbind(gadata[, 1:2], lpdata[1:2])
	time <- c(gadata[, 3], lpdata[3])
	normalized_data <- vector()
	for(i in 1:ncol(data)){
		min_value <- min(data[, i])
		max_value <- max(data[, i])
		a <- 0
		b <- 1
		#normalized_data <- cbind(normalized_data, (data[, i] - mean(data[, i])) / sd(data[, i]))
		normalized_data <- cbind(normalized_data, a + ((data[, i] - min_value) * (b - a) / (max_value - min_value)))
	}
	normalized_data <- cbind(normalized_data, time)
	normalized_data
}

#euclidean_distance <- function(vect, weight_for_cost = 0.5){
	#dist <- sqrt((weight_for_cost * vect[1])^2 + ((1 - weight_for_cost) * vect[2])^2)
	#dist
#}
