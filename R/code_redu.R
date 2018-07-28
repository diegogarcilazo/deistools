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


#library(tidyverse)
#
#con <- pgr::pg_con(mdb1252, driver = PostgreSQL)
#
#df <- as_tibble(pgr::pg_sql(con, 'SELECT ano,codmuer,edad,uniedad::INT FROM mortalidad.i01_v24'))
#
#
#df15 <- as_tibble(pgr::pg_sql(con, 'SELECT ano,codmuer,edad,uniedad::INT, grupedad FROM mortalidad.paisde15'))
#
#df <- mutate(df,
#       gedad = deistools::age_codeage(edad, uniedad)) %>%
#  filter(str_detect(gedad, 'M')) %>%
#  mutate(
#    redu = deistools::code_redu(ano, gedad, codmuer)
#  ) %>%
#  left_join(tbl_critred2, c('redu' = 'code_redu')) %>%
#  left_join(deistools::tbl_critred, c('redu' = 'code_redu')) %>%
#  select(-redu)
#
#df %>% group_by(ano, uniedad) %>% summarise(sum(edad == 0)) %>% print(n = 100)
#
#count(
#  mutate(df15, gedad = age_codeage(edad, uniedad)), edad, uniedad, grupedad, gedad)
#
#df <- mutate(df, gedad = deistools::age_codeage(edad, uniedad))
#
#filter(df, ano == 2015) %>%
#  mutate(M1 = if_else(str_detect(gedad, 'M'), 'M1', 'NO M1')) %>%
#  count(M1)
#
#
#t <- df %>% left_join(tbl_critred_labs)
#
#filter(t, ano == 2015) %>%
#  mutate(
#      gedad = case_when(
#      gedad %in% c('M1','M2') ~ 'Neo',
#      gedad %in% c('M3') ~ "Pos"
#    )) %>% count(gedad, key1, key2, key3) %>%
#  arrange(desc(gedad)) %>% print(n = 30)
#
#
#filter(df, ano == 2015) %>% count(critred)
#
#with(filter(t, ano == 2016), ftable(ano ~ key1 + key2 + key3,
#                                    na.action = na.pass,
#                                    exclude = NULL))
#?ftable
#table(t$key1)
#stack(table(t$key2))
#stack(table(t$key3))
#
#
#redu1 <- tibble(
#  lab = sort(unique(deistools::tbl_critred$critred)[-2],decreasing = T),
#  code = 1:13)
#
#
#tbl_critred_labs <- mutate(redu1,
#       key1 = if_else(str_detect(lab,'^REDUCIBLE |OTRAS REDUCIBLES'),
#                      'Reducible',
#                      str_to_title(lab)),
#       key2 = case_when(
#         str_detect(lab, 'EN EL RECIEN NACIDO') ~ 'En el Recién Nacido',
#         str_detect(lab, 'EN EL PERIODO PERINATAL') ~ 'En el Período Perinatal',
#         str_detect(lab, 'EN EL PARTO') ~ 'En el Parto',
#         str_detect(lab, 'EN EL EMBARAZO') ~ 'En el Embarazo',
#         str_detect(lab, 'OTRAS REDUCIBLES|REDUCIBLE POR TRATAMIENTO|REDUCIBLE POR PREVENCION Y TRATAMIENTO|REDUCIBLE POR PREVENCION') ~ 'Otras Reducibles'),
#       key3 = case_when(
#         str_detect(lab, 'POR TRATAMIENTO QUIRURGICO') ~ 'Por Tratamiento Quirúrgico',
#         str_detect(lab, 'POR TRATAMIENTO CLINICO Y QUIRURGICO') ~ 'Por Tratamiento Clínico y Quirúrgico',
#         str_detect(lab, 'POR TRATAMIENTO CLINICO') ~ 'Por Tratamiento Clínico')
#
#       ) %>% select(-1)
#
#
#tbl_critred2 <- left_join(deistools::tbl_critred, redu1,c('critred'= 'lab')) %>%
#  select(1,3)
#
#
#
#
#tibble(
#key2 = 1:7,
#lab2 = drop_na(tbl_critred_labs,key2) %>% pull(3))
#
#


