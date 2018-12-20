#' Function that classifies gestational weeks.
#' @param gestweeks int: gestational weeks.
#' @return factor.


gestational_age <- function(gestweeks){
  stopifnot(is.integer(gestweeks))

  if(any(gestweeks < 20)) {
    warning(paste0("There are ", sum( gestweeks < 20 ), " obs. less than 20sem. Will be coerse to NA."))
  }

  if(any(gestweeks > 46)) {
    warning(paste0("There are ", sum( gestweeks > 46 ), " obs. up to 47sem. Will be coerse to NA."))
  }

  return(
    cut(weight, breaks = c(19,28,32,37,43,47),
        labels = c('Prematuros Extremos (<28sem)',
                   'Muy Prematuros (28sem a 31sem)',
                   'Prematuros Moderados a Tardíos (32sem a 36sem)',
                   'A Término (37sem a 42sem)',
                   'Postérmino (>42sem)'),
        right = F)
  )
}
