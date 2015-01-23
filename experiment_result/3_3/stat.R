library(hypervolume)
read_ga_data <- function(){
	data <- list()
	for(iter in 1:40){
		filename <- paste(iter, ".csv", sep = "")
		temp <- read.csv(filename, sep = ',', header = T)
		temp <- temp[1:nrow(temp) - 1, ]
		filelist <- list(temp[, 1:2])
		data <- c(data, filelist)
	}
	data
}

read_lp_data <- function(){
	data <- list()
	for(iter in 1:40){
		filename <- paste(iter, ".csv", sep = "")
		temp <- read.csv(filename, sep = ',', header = T)
		temp <- temp[nrow(temp), ]
		filelist <- list(temp[, 1:2])
		data <- c(data, filelist)
	}
	data
}

read_ga_time <- function(){
	time <- vector()
	for(iter in 1:40){
		filename <- paste(iter, ".csv", sep = "")
		temp <- read.csv(filename, sep = ',', header = T)
		temp <- temp[1:nrow(temp) - 1, ]
		time <- c(time, temp$time[1])
	}
	time
}
read_lp_time <- function(){
	time <- vector()
	for(iter in 1:40){
		filename <- paste(iter, ".csv", sep = "")
		temp <- read.csv(filename, sep = ',', header = T)
		temp <- temp[nrow(temp), ]
		time <- c(time, temp$time[1])
	}
	time
}

statistic <- function(){
	ga_data <- read_ga_data()
	lp_data <- read_lp_data()
	ga_time <- read_ga_time()
	lp_time <- read_lp_time()

	lp_data_vec <- vector()
	band <- vector()
	#round <- vector()
	ga_hypervolume <- vector()
	lp_hypervolume <- vector()
	#standard <- vector()
	#gap <- vector()
	#print(mean(time))
	for(iter in 1:40){
		#round <- c(round, mean(data[[iter]]))
		band <- estimate_bandwidth(ga_data[[iter]])
		ga_hypervolume <- c(ga_hypervolume, get_volume(hypervolume(ga_data[[iter]], bandwidth = band, warnings = F)))
		band <- estimate_bandwidth(lp_data[[iter]])
		lp_hypervolume <- c(lp_hypervolume, get_volume(hypervolume(lp_data[[iter]], bandwidth = 0.01, warnings = F)))
		#gap <- c(gap, minima[iter] - ala_result)
		}
	cat("GA average time", mean(ga_time), '\n')
	cat("LP average time", mean(lp_time), '\n')
	cat("GA average hypervolume", mean(ga_hypervolume), '\n')
	cat("LP average hypervolume", mean(lp_hypervolume), '\n')
	cat("Max gap", mean(ga_hypervolume) - mean(lp_hypervolume), '\n')
	cat("Sd", sd(ga_hypervolume), '\n')
	}
