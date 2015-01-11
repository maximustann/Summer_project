
city <- c('A', 'B', 'C', 'D')
service <- c('S1', 'S2', 'S3', 'S4')

cost_matrix <- matrix(data = c(60, 100, 135, 80, 130, 210, 40, 95, 90, 120, 140, 95, 63, 53, 63, 123), nrow = 4, ncol = 4)
set.seed(1)
frequency_matrix <- matrix(data = floor(runif(4 * 4, 1, 120)), nrow = 4, ncol = 4)
latency_matrix <- matrix(data = c(5, 2, 1, 5, 2, 1, 7, 
								  2, 2, 1, 2, 2, 4, 3, 7, 4), nrow = 4, ncol = 4)


