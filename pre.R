
city <- c('A', 'B', 'C', 'D')
service <- c('S1', 'S2', 'S3')

cost_matrix <- matrix(data = rnorm(3 * 4, mean = 100, sd = 20), nrow = 3, ncol = 4)
frequency_matrix <- matrix(data = rnorm(3 * 4, mean = 100, sd = 80), nrow = 3, ncol = 4)
latency_matrix <- matrix(data = c(5.982, 2.130, 0.71, 5.776, 0.74, 0.76, 6.984, 
								  2.035, 1.863, 0.639, 0.68, 0.672), nrow = 3, ncol = 4)


