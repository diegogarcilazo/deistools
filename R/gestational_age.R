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
    cut(gestweeks, breaks = c(19,28,32,37,43,47),
        labels = c('Extremely preterm (<28sem)',
                   'Very preterm (28sem a 31sem)',
                   'Moderate to late preterm (32sem a 36sem)',
                   'Mature births (37sem a 42sem)',
                   'Post mature births (>42sem)'),
        right = F)
  )
}
