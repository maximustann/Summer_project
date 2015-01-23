plotme <- function(condition, number = 1){
	
	filename <- paste(condition, "/", number, ".csv", sep = "")
	ga_data <- read_ga_data(filename)
	lp_data <- read_lp_data(filename)
	plot(ga_data, xlim = 0:1, ylim = 0:1, col = 'blue', xlab = 'cost', ylab = 'latency')
	par(new = T)
	plot(lp_data, xlim = 0:1, ylim = 0:1, col = 'red', pch = 4)


}

read_ga_data <- function(filename){
	data <- read.csv(filename, sep = ',', header = T)
	return(data[1:(nrow(data) - 1), 1:2])
}

read_lp_data <- function(filename){
	data <- read.csv(filename, sep = ',', header = T)
	return(data[nrow(data), 1:2])
}

