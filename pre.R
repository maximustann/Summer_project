
city <- c('A', 'B', 'C', 'D')
service <- c('S1', 'S2', 'S3')

cost_matrix <- matrix(data = c(60, 100, 135, 80, 130, 210, 40, 95, 90, 120, 140, 95), nrow = 3, ncol = 4)
set.seed(2)
frequency_matrix <- matrix(data = floor(runif(3 * 3, 1, 120)), nrow = 3, ncol = 3)
latency_matrix <- matrix(data = c(5, 2, 1, 5, 2, 1, 7, 
								  2, 2, 1, 2, 2), nrow = 3, ncol = 4)


