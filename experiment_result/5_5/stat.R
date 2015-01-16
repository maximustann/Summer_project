readdata <- function(){
	data <- list()
	for(iter in 1:40){
		filename <- paste(iter, ".csv", sep = "")
		temp <- read.csv(filename, sep = ',', header = T)
		filelist <- list(temp$dist)
		data <- c(data, filelist)
		#data_time <- c(data_time, temp$time[1])
	}
	data
}

readtime <- function(){
	time <- vector()
	for(iter in 1:40){
		filename <- paste(iter, ".csv", sep = "")
		temp <- read.csv(filename, sep = ',', header = T)
		time <- c(time, temp$time[1])
	}
	time
}


statistic <- function(ala_result){
	data <- readdata()
	time <- readtime()
	round <- vector()
	minima <- vector()
	standard <- vector()
	gap <- vector()
	print(mean(time))
	for(iter in 1:40){
		round <- c(round, mean(data[[iter]]))
		minima <- c(minima, min(data[[iter]]))
		standard <- c(standard, sd(data[[iter]]))
		for(res_iter in 1:length(data[[iter]]))
			gap <- c(gap, ala_result - data[[iter]][res_iter])
		}
	print(mean(round))
	print(min(minima))
	print(mean(standard))
	print(mean(gap))
	}
