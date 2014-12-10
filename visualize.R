visualize <- function(num_0, num_1 = F){
	if(num_1 != F){
		for(iter in num_0:num_1){
			Visualize(iter)
			Sys.sleep(0.25)
		}
	}
	else{
		Visualize(num_0)
	}
}

Visualize <- function(num){
	#first step, Read 3 files
	#construct the names
	particle_name <- paste("particles_", num, ".CSV", sep = '')
	connection_name <- paste("connections_", num, ".CSV", sep = '')
	archive_name <- paste("archive_", num, ".CSV", sep = '')

	#read files
	particle <- as.matrix(read.csv(particle_name))
	connection <- as.matrix(read.csv(connection_name), header = F)
	archive <- as.matrix(read.csv(archive_name))

	#fit an error
	connection[, 2] <- connection[, 2] + 1

	#plot
	plot(particle, xlim = c(0, 13), ylim = c(0, 1))
	par(new = T)
	plot(archive, xlim = c(0, 13), ylim = c(0, 1), col = 'red', pch = 25, bg = 'red')
	for(iter in 1:nrow(connection)){
		arrows(x0 = as.numeric(particle[connection[iter, 1], 1]), y0 = as.numeric(particle[connection[iter, 1], 2]),
			   x1 = as.numeric(archive[connection[iter, 2], 1]), y1 = as.numeric(archive[connection[iter, 2], 2]),
			   code = 0, col = 'gray')
		#par(new = T)
	}
}
