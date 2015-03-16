stat <- function(dataset, condition){
	data <- read_file(dataset, condition)
	nsga_time <- data[1:(nrow(data) - 40), ]
	ga_time <- data[(nrow(data) - 40 + 1):nrow(data), ]
	mean_nsga <- mean(nsga_time)
	mean_ga <- mean(ga_time)
	sd_nsga <- sd(nsga_time)
	sd_ga <- sd(ga_time)
	cat("Mean NSGA: ", mean_nsga, "SD: ", sd_nsga, "\n")
	cat("Mean GA: ", mean_ga, "SD: ", sd_ga, "\n")
}

read_file <- function(dataset, condition){
	filename <- paste(dataset, '_', dataset, '/', condition, '/', dataset, '_time.csv', sep = "")
	data <- read.csv(filename, sep = ",", header = T)
	data
}
