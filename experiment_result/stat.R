stat <- function(dataset, condition){
	data <- read_file(dataset, condition)
	ga_time <- data[1:(nrow(data) - 1), ]
	lp_time <- data[nrow(data), ]
	mean_ga <- mean(ga_time)
	sd_ga <- sd(ga_time)
	print(mean_ga)
	print(sd_ga)
	print(lp_time)
}

read_file <- function(dataset, condition){
	filename <- paste(dataset, '_', dataset, '/', condition, '/', dataset, '_time.csv', sep = "")
	data <- read.csv(filename, sep = ",", header = T)
	data
}
