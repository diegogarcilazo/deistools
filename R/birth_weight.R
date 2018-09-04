#' Function that classifies birth weight.
#' @param weight int: birth weight in gr.
#' @return factor.


birth_weight <- function(weight){
	stopifnot(is.integer(weight))
	
	if(any(weight < 500)) {
		warning(paste0("There are ", sum( weight < 500 ), " obs. less than 500g. Will be coerse to NA."))
	}
	
	if(any(weight > 6000)) {
		warning(paste0("There are ", sum( weight > 6000 ), " obs. up to 6000g. Will be coerse to NA."))
	}
	
	return(
	cut(weight, breaks = c(500,1000,1500,2500,4000,6000),
			labels = c('Extremely low birth weight (ELBW)', 
								 'Very low birth weight (VLBW)',
								 'Low birth weight (LBW)', 
								 'Normal birth weight', 
								 'Macrosomia'), 
			right = F)
	)
}
