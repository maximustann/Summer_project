

source("DataExtraction.R")
predata <- function(matrixSize){
	#city <- c('A', 'B', 'C', 'D')
	#service <- c('S1', 'S2', 'S3', 'S4')

	#cost_matrix <- matrix(data = c(60, 100, 135, 80, 130, 210, 40, 95, 90, 120, 140, 95, 63, 53, 63, 123), nrow = 4, ncol = 4)
	set.seed(1)
	#generate a cost matrix which has the average number of 100 and its standard deviation is 20.
	cost_matrix <<- matrix(floor(rnorm(matrixSize * matrixSize, 100, 20)), 
						  nrow = matrixSize, ncol = matrixSize)

	#generate a frequency matrix, which ranging from 1 to 120.
	frequency_matrix <<- matrix(data = floor(runif(matrixSize * matrixSize, 1, 120)), 
							   nrow = matrixSize, ncol = matrixSize)

	latency_matrix <<- extract(matrixSize)
}

