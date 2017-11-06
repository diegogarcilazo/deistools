library(pgr)
library(tidyverse)

devtools::install_github('diegogarcilazo/biotools')

devtools::use_package("tidyverse")
devtools::use_package("readxl")
devtools::use_package("stringr")



library(biotools)



lookup_discharge$tippart <-
  rename(terminac, codtippart = CodTerm, terminac = Terminac) %>%
  mutate_all(
    function(x) toupper(myutilities::acc_rm(x))
  )

list_tbls <- map(list_tbls,
  function(y) mutate_all(y,
    function(x) toupper(myutilities::acc_rm(x))
  ))

tbl_critred <- tbl_critred %>%
  mutate_all(
    function(x) toupper(myutilities::acc_rm(x))
  )

lookup_discharge$localidad <- localidad %>%
  mutate_all(
    function(x) toupper(myutilities::acc_rm(x))
  )

localidad <- as_tibble(
  stringr::str_split(
    stringr::str_subset(
      lookup_discharge$establec$est_localidad,'[0-9]{3}'),' ', n = 2,simplify = T)
)



library(biotools)

devtools::document()

critred15 <- readxl::read_excel('data/CritReduc.xls')

library(tidyverse)

critred15 %>%
  pull(REDUCIB) %>% unique %>% paste0("'",.,"'") %>% cat(sep = '\n')

critred15 %>%
  count(NEOPOS)

tbl_critred %>% pull(critred1011) %>%  unique %>% paste0("'",.,"'") %>% cat(sep = '\n')
tbl_critred %>% count(grupedad)

critred15b <- critred15 %>%
transmute(
codmuer = CODMUER,
critred15 = case_when(
  REDUCIB == '31. Red rn clinico' ~ 'REDUCIBLE EN EL RECIEN NACIDO POR TRATAMIENTO CLINICO',
  REDUCIB == '10. Red embarazo' ~'REDUCIBLE EN EL EMBARAZO',
  REDUCIB == '70. No clasif' ~ 'NO CLASIFICABLES',
  REDUCIB == '90. Mal definidas' ~ 'MAL DEFINIDAS',
  REDUCIB == '20. Red parto' ~ 'REDUCIBLE EN EL PARTO',
  REDUCIB == '40. Otras red' ~ 'OTRAS REDUCIBLES',
  REDUCIB == '35. Red per perinatal' ~ 'REDUCIBLE EN EL PERIODO PERINATAL',
  REDUCIB == '50. Dificil red' ~ 'DIFICILMENTE REDUCIBLE',
  REDUCIB == '32. Red rn quir' ~ 'REDUCIBLE EN EL RECIEN NACIDO POR TRATAMIENTO QUIRURGICO',
  REDUCIB == '33. Red rn clin y quir' ~ 'REDUCIBLE EN EL RECIEN NACIDO POR TRATAMIENTO CLINICO Y QUIRURGICO',
  REDUCIB == '30. Red por Prev y Trat' ~ 'REDUCIBLE POR PREVENCION Y TRATAMIENTO',
  REDUCIB == '10. Red por Prev' ~ 'REDUCIBLE POR PREVENCION',
  REDUCIB == '20. Red por Trat' ~'REDUCIBLE POR TRATAMIENTO',
  T ~ NA_character_),
grupedad = case_when(
  NEOPOS == 'NEO' ~ 'NEONATAL',
  NEOPOS == 'POS' ~ 'POSNEONATAL',
  T ~ NA_character_
))


devtools::install_github('diegogarcilazo/biotools')

library(biotools)

tbl_critred %>%
  anti_join(critred15b, c('grupedad', 'codmuer'))


critred15b %>%
  count(grupedad)

critred15b %>%
  count(critred15)

library(tidyverse)
library(stringr)
library(biotools)

con <- pgr::pg_con(mdb1252)

def_inf_i08 <- tbl(con, dbplyr::in_schema('mi','def_inf_i08'))

reduneo <- tbl(con, dbplyr::in_schema('codigos','reduneo')) %>% collect

tbl_critred

def_inf_i08 %>% collect %>%
  mutate(critred = code_redu(ano, grupedad, codmuer)) %>%
  left_join(tbl_critred) %>%
  count(ano,
        edad = case_when(str_detect(grupedad, 'M1|M2') ~ 'NEONATAL',
                         str_detect(grupedad, 'M3') ~ 'POSNEONATAL',
                         T ~ NA_character_),
        critred) %>%  print(n = 200)
