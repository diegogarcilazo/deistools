#' This is a helper function. This function builds the code_redu
#' by joining the variables year and group age to later join with the tbl_critred.
#' @param year Year
#' @param grupedad Group age with modes M1(0-6 days), M2(7-28 days) and M3(29d-11m).
#' @param codmuer death code.
#' @return code_redu for join with tbl_critred.


code_redu <- function(year, grupedad, codmuer)
{

  dplyr::case_when(
    (year >= 2012 & year <= 2016) & grupedad%in%c('M1','M2') ~ paste0(codmuer,12151L),
    (year >= 2012 & year <= 2016) & grupedad%in%c('M3') ~ paste0(codmuer,12152L),
    (year >= 2010 & year <= 2011) & grupedad%in%c('M1','M2') ~ paste0(codmuer,10111L),
    (year >= 2010 & year <= 2011) & grupedad%in%c('M3') ~ paste0(codmuer,10112L),
    T ~ NA_character_)

}



#' join with the tbl_critred2.
#' @param year Year
#' @param age age.
#' @param codeage Group age with modes M1(0-6 days), M2(7-28 days) and M3(29d-11m).
#' @param codmuer death code.
#' @return tibble.


reductible <- function(df, age, codeage, year, codmuer, ...){

  age = dplyr::enquo(age)
  codeage = dplyr::enquo(codeage)
  year = dplyr::enquo(year)
  codmuer = dplyr::enquo(codmuer)
  id = dplyr::quos(...)

  df %<>%
    dplyr::mutate(
      gedad = deistools::age_codeage(!!age, !!codeage)
    ) %>%
    dplyr::filter(stringr::str_detect(gedad, 'M')) %>%
    dplyr::mutate(
      redu = deistools::code_redu(!!year, gedad, !!codmuer),
      gedad2 = dplyr::case_when(
        gedad %in% c('M1','M2') ~ 'Neonatal',
        gedad %in% c('M3') ~ "Posneonatal")
    ) %>%
    dplyr::left_join(deistools::tbl_critred2, c('redu' = 'code_redu')) %>%
    dplyr::select(-redu) %>%
    dplyr::left_join(deistools::tbl_critred_labs) %>%
    dplyr::select(-code)
  redu_summary(df)
  return(df)
}



#' join with the tbl_critred2.
#' @param tblReductible tbl result from reductible.

redu_summary.data.frame <- function(t){
  cat("
  ---------------------------------------------
  Criterios de Reducibilidad:

        " %>% glue::glue(.envir = list()))

  print(rbind(
    xtabs( ~ key1 + gedad2, t),
    Total = xtabs( ~ gedad2, t)
  ))
  cat("
  ---------------------------------------------
  Reducibles:

        " %>% glue::glue(.envir = list()))
  print(
    rbind(
      xtabs( ~ key2 + gedad2, t),
      Total = xtabs( ~ gedad2, t, subset = key1 == 'Reducible')))
  cat("
  ---------------------------------------------
  En el Recién Nacido:

        " %>% glue::glue(.envir = list()))
  print(
    rbind(
      xtabs( ~ key3 + fct_expand(gedad2), t),
      Total = xtabs( ~ fct_expand(gedad2), t, subset = key2 == 'En el Recién Nacido'))
  )
  invisible()
}

redu_summary <-  function (x, ...) UseMethod('redu_summary', x);

