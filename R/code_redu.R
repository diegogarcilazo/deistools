#' function for recode critred
code_redu <- function(year, grupedad, codmuer)
{

  dplyr::case_when(
    (year >= 2012 & year <= 2015) & grupedad%in%c('M1','M2') ~ paste0(codmuer,12151L),
    (year >= 2012 & year <= 2015) & grupedad%in%c('M3') ~ paste0(codmuer,12152L),
    (year >= 2010 & year <= 2011) & grupedad%in%c('M1','M2') ~ paste0(codmuer,10111L),
    (year >= 2010 & year <= 2011) & grupedad%in%c('M3') ~ paste0(codmuer,10112L),
    T ~ NA_character_)


}

