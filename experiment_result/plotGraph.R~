plotme <- function(condition){
	filename <- paste("./performance/", condition, ".csv", sep = "")
	data <- read.csv(filename, sep = ',', header = T)
	attach(data)
	plot(NSGA.II, ylim = 0:1, col = 'blue', xlab = 'Matrix Size', ylab = 'Distance', xaxt='n', type='b', pch = 4)
	axis(1, at=c(1, 2, 3, 4), labels = MatrixSize)
	par(new = T)
	plot(ALA, ylim = 0:1, col = 'red', type = 'b', xaxt = 'n', xlab = 'Matrix Size', ylab = 'Distance', pch = 20)
	legend("topright", c("NSGA-II", "ALA"), col = c("blue", "red"), pch = c(4, 20))

}
