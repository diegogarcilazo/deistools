#' Check completeness from a vector of known cats.
#' @param name: chr name of var.
#' @param var: vector with data.
#' @param cats: vector with categories

completeness_tbl <- function(name, var, cats){

  tibble(
    Name = name,
    Correct = sum(var%in%cats),
    NAs = sum(is.na(var)),
    n = length(var)
  ) %>% transmute(
    Name,
    Correct,
    Unknown = n - (Correct + NAs),
    NAs,
    pct_correct = round(Correct * 100 /
                          (Correct + Unknown + NAs),1)
  )
}

