#' Fill with left zeros 
#' @param int integer
#' @param n int length of character  
#' @return character of length \code{n} filled with 0 on the left

zerofill <- function(int, nceros){
  resultado<-gettextf(paste("%0",nceros,"i", sep = ''),as.numeric(int));
  return(resultado);
}

