plotme <- function(condition, number = 3){
	if(number == 3){
		filename <- paste("3_3", "/", condition, "/", number, ".csv", sep = "")
		initialisation_filename <- paste("3_3", "/", condition, "/", number, "_initialisation.csv", sep = "")
	}
	else if(number == 5){
		filename <- paste("5_5", "/", condition, "/", number, ".csv", sep = "")
		initialisation_filename <- paste("5_5", "/", condition, "/", number, "_initialisation.csv", sep = "")
	}
	else if(number == 10){
		filename <- paste("10_10", "/", condition, "/", number, ".csv", sep = "")
		initialisation_filename <- paste("10_10", "/", condition, "/", number, "_initialisation.csv", sep = "")
	}
	else if(number == 15){
		filename <- paste("15_15", "/", condition, "/", number, ".csv", sep = "")
		initialisation_filename <- paste("15_15", "/", condition, "/", number, "_initialisation.csv", sep = "")
	}
	else if(number == 20){
		filename <- paste("20_20", "/", condition, "/", number, ".csv", sep = "")
		initialisation_filename <- paste("20_20", "/", condition, "/", number, "_initialisation.csv", sep = "")
	}
	nsga_data <- read_nsga_data(filename)
	ga_data <- read_ga_data(filename)
	initialisation_nsga_data <- read_nsga_data(initialisation_filename)
	initialisation_ga_data <- read_ga_data(initialisation_filename)
	#left_x <- min(c(min(nsga_data$costF), min(ga_data$costF)))
	#right_x <- max(c(max(nsga_data$costF), max(ga_data$costF)))
	#left_y <- min(c(min(nsga_data$latencyF), min(ga_data$latencyF)))
	#right_y <- max(c(max(nsga_data$latencyF), max(ga_data$latencyF)))
	par(font.axis = 2)
	#plot(nsga_data[order(nsga_data$latencyF),], xlim = c(left_x, right_x), ylim = c(left_y, right_y), col = 'blue', xlab = 'cost', ylab = 'latency',  pch = 20, type = 'b', font.lab = 2)
	plot(nsga_data[order(nsga_data$latencyF),], xlim = c(0, 0.5), ylim = c(0, 0.7), col = 'blue', xlab = 'cost', ylab = 'latency',  pch = 15, type = 'b', font.lab = 2)
	par(new = T)
	#plot(ga_data, xlim = c(left_x,right_x), ylim = c(left_y, right_y), col = 'red', xlab = '', ylab = '', pch = 4)
	plot(ga_data, xlim = c(0, 0.5), ylim = c(0, 0.7), col = 'red', xlab = '', ylab = '', pch = 1)
	par(new = T)
	plot(initialisation_nsga_data[order(initialisation_nsga_data$latencyF),], xlim = c(0, 0.5), ylim = c(0, 0.7), col = 'green', xlab = '', ylab = '', pch = 17, type = 'b')
	par(new = T)
	plot(initialisation_ga_data, xlim = c(0, 0.5), ylim = c(0, 0.7), col = 'black', xlab = '', ylab = '', pch = 4)

	par(xpd = T)
	legend("topright",c("NSGA-II", "GA", "Initialized NSGA", "Initialized GA"), col = c("blue", "red", "green", "black"), pch = c(15, 1, 17, 4))

}

read_nsga_data <- function(filename){
	data <- read.csv(filename, sep = ',', header = T)
	return(data[1:(nrow(data) - 40),])
}

read_ga_data <- function(filename){
	data <- read.csv(filename, sep = ',', header = T)
	return(data[(nrow(data) - 40 + 1):nrow(data),])
}

