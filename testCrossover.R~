test <- function(){
	pop <- vector()
	for(i in 1:30){
		pop <- rbind(pop, rbinom(12, 1, 0.5))	
	}

	print(pop)
	new_pop <- crossover(pop, 0.7)
	print(nrow(new_pop))
}
