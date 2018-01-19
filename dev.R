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



devtools::install_github('diegogarcilazo/biotools')

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

def_inf_i08 %>% collect %>%
  mutate(code_redu = code_redu(ano, grupedad, codmuer)) %>%
  left_join(tbl_critred) %>%
  count(ano,
        edad = case_when(str_detect(grupedad, 'M1|M2') ~ 'NEONATAL',
                         str_detect(grupedad, 'M3') ~ 'POSNEONATAL',
                         T ~ NA_character_),
        critred) %>%  print(n = 200)


library(tidyverse)

lkuptbls_deis2016 <- map(
  c("Depart","País","ClasiEnf","Variables del IED"),
  readxl::read_excel,
  path = 'data/USUDEF16.xls') %>%
  setNames(c("Depart","País","ClasiEnf",'IED' ))

lkuptbls_deis2016$IED %>% print(n = 300)

lkuptbls_deis2016$JURI <- lkuptbls_deis2016$IED[5:28, 5:6]
lkuptbls_deis2016$ATENMED <- lkuptbls_deis2016$IED[34:36, 5:6]
lkuptbls_deis2016$MEDSUS <- lkuptbls_deis2016$IED[40:42, 5:6]
lkuptbls_deis2016$MAT <- lkuptbls_deis2016$IED[48:49, 5:6]
lkuptbls_deis2016$UNIEDAD <- lkuptbls_deis2016$IED[59:63, 5:6]
lkuptbls_deis2016$SEXO <- lkuptbls_deis2016$IED[67:70, 5:6]
lkuptbls_deis2016$OCLOC <- lkuptbls_deis2016$IED[74:78, 5:6]
lkuptbls_deis2016$ASOCIAD <- lkuptbls_deis2016$IED[145:149, 5:6]
lkuptbls_deis2016$INSTRUC <- lkuptbls_deis2016$IED[153:166, 5:6]
lkuptbls_deis2016$SITLABOR <- lkuptbls_deis2016$IED[170:173, 5:6]
lkuptbls_deis2016$MSITCONY <- lkuptbls_deis2016$IED[197:199, 5:6]

lkuptbls_deis2016$VARS <- lkuptbls_deis2016$IED %>%
  select(CAMPO, VARIABLE, TIPO) %>%
  drop_na()


lkuptbls_old <- list_tbls[c('loc_bio','muervio','estabio','embmujer','deptos')]
devtools::use_data(lkuptbls_deis2016,lkuptbls_old,lookup_discharge,tbl_cie10,
                   tbl_critred,list_tbls, internal = T, overwrite = T)


lkuptbls_deis2016$ClasiEnf %>%
  mutate(
    useless = deistools::code_useless(SubCat)
  ) %>%
  filter(str_detect(useless, 'Tipo')) %>%
  print(n = 1000)

tbl_cie10 %>%
  mutate(
    useless2 = code_useless(code)
  ) %>%
  filter(useless == 0 & str_detect(useless2, 'Tipo') |
           useless >=1 & is.na(useless2),
         str_length(code) == 4) %>%
  select(code, entity, useless, useless2)


tbl_cie10 %>%
  count(suspected_maternal_death)


tbl_cie10 %>%
  anti_join(
    lkuptbls_deis2016$ClasiEnf, c('code' = 'SubCat')
  ) %>%
  filter(str_length(code) == 4)

lkuptbls_deis2016$ClasiEnf %>%
  filter(str_detect(SubCat, 'C83'))

tbl_cie10 %>%
  filter(str_detect(code, 'A09'))

