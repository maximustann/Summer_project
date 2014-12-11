
city <- c('A', 'B', 'C', 'D')
service <- c('S1', 'S2', 'S3')

cost_matrix <- matrix(data = c(60, 100, 134, 80, 130, 210, 40, 93, 88, 120, 142, 92), nrow = 3, ncol = 4)
#set.seed(2)
frequency_matrix <- matrix(data = floor(runif(3 * 3, 1, 120)), nrow = 3, ncol = 3)
latency_matrix <- matrix(data = c(5.982, 2.130, 0.71, 5.776, 0.74, 0.76, 6.984, 
								  2.035, 1.863, 0.639, 0.68, 0.672), nrow = 3, ncol = 4)


