source("pre.R")
source("algorithm.R")
source("greedy.R")
library(GA)
library(mco)
library(nsga2R)
library(lpSolve)
library(foreach)
library(doMC)
registerDoMC(8)

run <- function(matrixSize, cost_limitation){
	gadata <- vector()
	lpdata <- vector()
	gatime<- vector()
	lptime<- vector()
	foreach(iter = 1:40) %dopar%{
		cat("Generation:", iter, '\n')
		gadata <- run_algorithm(matrixSize, iter, cost_limitation)
		lpdata <- run_greedy(matrixSize, iter, cost_limitation)

		lptime <- lpdata[3]
		lpdata <- matrix(lpdata, nrow = 1)
		colnames(lpdata) <- c("costF", "latencyF", "time")
		lpdata <- data.frame(lpdata)
		filename_lp <- paste(iter, "_lp.csv", sep = "")
		filename_lp_time <- paste(iter, "_time_lp.csv", sep = "")
		write.csv(lpdata[, 1:2], filename_lp, quote = F, row.names = F, sep = ",", col.names = T)
		write.csv(lptime, filename_lp_time, quote = F, row.names = F, col.names = F)

		gatime <- gadata[1, 3]
		filename_ga <- paste(iter, "_ga.csv", sep = "")
		filename_ga_time <- paste(iter, "_time_ga.csv", sep = "")
		write.csv(gadata[, 1:2], filename_ga, quote = F, row.names = F)
		write.csv(gatime, filename_ga_time, quote = F, row.names = F)
	}

	for(iter in 1:40){
		filename_ga <- paste(iter, "_ga.csv", sep = "")
		filename_ga_time <- paste(iter, "_time_ga.csv", sep = "")
		gadata <- rbind(gadata, read.csv(filename_ga, header = T, sep = ','))
		gatime <- rbind(gatime, read.csv(filename_ga_time, header = T, sep = ","))

		filename_lp <- paste(iter, "_lp.csv", sep = "")
		filename_lp_time <- paste(iter, "_time_lp.csv", sep = "")
		lpdata <- rbind(lpdata, read.csv(filename_lp, header = T, sep = ','))
		lptime <- rbind(lptime, read.csv(filename_lp_time, header = T, sep = ","))
	}

	#lptime <- read.csv("1_time_lp.csv", header = T)
	time_data <- rbind(gatime, lptime)
	best_ga <- best_ga_front(gadata)
	cat("row for best GA data: ", nrow(best_ga), "\n")
	cat("row for LP data: ", nrow(lpdata), "\n")
	#lpdata <- read.csv("1_lp.csv", sep = ",", header = T)
	data <- total_normalised(best_ga, lpdata)[, 1:2]
	colnames(data) <- c("costF", "latencyF")
	filename <- paste(matrixSize, ".csv", sep = "")
	filename_time <- paste(matrixSize, "_time.csv", sep="")
	write.csv(data, filename, quote = F, row.names = F)
	write.csv(time_data, filename_time, quote = F, row.names = F)
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
