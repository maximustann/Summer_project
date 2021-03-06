source("DataExtraction.R")
predata <- function(matrixSize){
	set.seed(1)
	cost_matrix <<- matrix(floor(rnorm(matrixSize * matrixSize, 100, 20)), 
						  nrow = matrixSize, ncol = matrixSize)

	frequency_matrix <<- matrix(data = floor(runif(matrixSize * matrixSize, 1, 120)), 
							   nrow = matrixSize, ncol = matrixSize)

	latency_matrix <<- extract(matrixSize)
}

