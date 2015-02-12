plotme <- function(condition, number = 3){
	
	if(number == 3){
		filename <- paste("3_3", "/", condition, "/", number, ".csv", sep = "")
	}
	else if(number == 5){
		filename <- paste("5_5", "/", condition, "/", number, ".csv", sep = "")
	}
	else if(number == 10){
		filename <- paste("10_10", "/", condition, "/", number, ".csv", sep = "")
	}
	else if(number == 15){
		filename <- paste("15_15", "/", condition, "/", number, ".csv", sep = "")
	}
	else if(number == 20){
		filename <- paste("20_20", "/", condition, "/", number, ".csv", sep = "")
	}
	ga_data <- read_ga_data(filename)
	lp_data <- read_lp_data(filename)
	par(font.axis = 2)
	plot(ga_data[order(ga_data$latencyF),], xlim = 0:1, ylim = 0:1, col = 'blue', xlab = 'cost', ylab = 'latency',  pch = 4, type = 'b', font.lab = 2)
	par(new = T)
	plot(lp_data, xlim = 0:1, ylim = 0:1, col = 'red', xlab = '', ylab = '', pch = 20, cex = 2)

	legend("topright", c("NSGA-II", "ALA"), col = c("blue", "red"), pch = c(4, 20))

}

read_ga_data <- function(filename){
	data <- read.csv(filename, sep = ',', header = T)
	return(data[1:(nrow(data) - 1),])
}

read_lp_data <- function(filename){
	data <- read.csv(filename, sep = ',', header = T)
	return(data[nrow(data),])
}

