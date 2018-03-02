library(pgr)
library(tidyverse)
library(deistools)

devtools::install_github('diegogarcilazo/deistools')

devtools::use_package("tidyverse")
devtools::use_package("readxl")
devtools::use_package("stringr")

devtools::document()
devtools::use_test("testing")
devtools::use_testthat()


con <- pgr::pg_con(mdb1252)

def_inf_i08 <- tbl(con, dbplyr::in_schema('mi','def_inf_i08'))

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



critred15 <- readxl::read_excel('data/CritReduc.xls')

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


#Armado de lkup tbls de la DEIS para NV.

lkuptbls_nv <- map(
  c("Depart","País","Variables del IENV"),
  readxl::read_excel,
  path = '/home/diego/Documentos/datasets/USUNAC16.xls') %>%
  setNames(c("Depart","País","IENV"))

lkup_def_deis <- lkuptbls_deis2016
lkup_nv_deis <- lkuptbls_nv

lkuptbls_nv$JURI <- lkuptbls_nv$IENV[5:28, 5:6]
lkuptbls_nv$ATPARTO <- lkuptbls_nv$IENV[58:65, 5:6]
lkuptbls_nv$TIPPARTO <- lkuptbls_nv$IENV[46:48, 5:6]
lkuptbls_nv$PROVRES <- lkuptbls_nv$IENV[79:103, 5:6]
lkuptbls_nv$PROVOC <- lkuptbls_nv$IENV[79:103, 5:6]
lkuptbls_nv$SEXO <- lkuptbls_nv$IENV[37:40, 5:6]
lkuptbls_nv$OCLOC <- lkuptbls_nv$IENV[69:73, 5:6]
lkuptbls_nv$MASOCIAD <- lkuptbls_nv$IENV[166:170, 5:6]
lkuptbls_nv$PASOCIAD <- lkuptbls_nv$IENV[197:201, 5:6]
lkuptbls_nv$PINSTRUC <- lkuptbls_nv$IENV[180:193, 5:6]
lkuptbls_nv$MINSTRUC <- lkuptbls_nv$IENV[149:162, 5:6]
lkuptbls_nv$SITLABOR <- lkuptbls_nv$IENV[205:208, 5:6]
lkuptbls_nv$MSITCONY <- lkuptbls_nv$IENV[174:176, 5:6]


lkuptbls_nv$VARS <- lkuptbls_nv$IENV %>%
  drop_na(CAMPO) %>%
  select(CAMPO, VARIABLE, TIPO, ANCHO) %>%
  mutate(
    is. = case_when(
      TIPO == 'TEXTO' ~ 'is.character',
      TIPO == 'NUMERICO' ~ 'is.integer',
      T ~ TIPO
    ),
    ANCHO = as.integer(ANCHO)) %>%
  left_join(
    map(names(lkuptbls_nv[c(4:16)]),
        ~ lkuptbls_nv[[.x]] %>%
          mutate(
            CAMPO = .x,
          )) %>%
      bind_rows(), 'CAMPO') %>%
  group_by(CAMPO, VARIABLE, is.) %>%
  nest(CODIGOS, DESCRIPCION, .key = 'CLASS') %>%
  mutate(
    CLASS = case_when(
      CAMPO == 'DEPRES' ~ list(lkuptbls_nv$Depart),
      CAMPO == 'DEPOC' ~ list(lkuptbls_nv$Depart),
      CAMPO == 'PROVRES' ~ list(lkuptbls_nv$JURI),
      CAMPO == 'PROVOC' ~ list(lkuptbls_nv$JURI),
      CAMPO == 'PAISRES' ~ list(lkuptbls_nv$País),
      is. == 'is.integer' ~ list(NA_integer_),
      str_detect(VARIABLE, 'FECHA') ~ list(NA_character_),
      T ~ CLASS)) %>% print(n = 100)

lkuptbls_deis2016$VARS <- lkuptbls_nv$IENV %>%
  select(CAMPO, VARIABLE, TIPO) %>%
  drop_na() %>% print(n = 100)


enos <- readxl::read_excel('data/enos2cie10.xlsx')

enos2 <-
  enos %>%
  mutate(
   group = case_when(
     !str_detect(EVENTOS, "[0-9]") ~ EVENTOS),
   group = zoo::na.locf(lag(group), na.rm = F)) %>%
  filter(str_detect(EVENTOS, "[0-9]")) %>%
  transmute(
       EVENTOS = str_replace(EVENTOS, "[0-9]\\. ", ''),
      CODIGO = str_replace_all(`capitulo I al XIX`, '[\\.,]',''),
     CODIGO = if_else(is.na(CODIGO), `CAUSA EXTERNA (CAPITULO XX)`,CODIGO),
     CODIGO = str_replace_all(CODIGO, ' ', ''),
     CODIGO = if_else(EVENTOS == 'BRONQUIOLITIS', 'J21', CODIGO),
     EVENTOS = paste(row_number(), '-',EVENTOS)
     )


enos2 %>%
  filter(str_detect(CODIGO,'-')) %>%
  separate(CODIGO, c('From','To')) %>%
  separate(To, c('To','except'), 'excepto') %>%
  mutate(
    except = str_replace(except, 'e', ', '),
    From = str_pad(From, 4, 'right', '0'),
    To = str_pad(To, 4, 'right', 'X'),
    range = paste0("(x >= '", From, "' & ", "x <= '", To, "')"),
    exception = paste0("!x %in% c(", except, ")"),
    code = if_else(is.na(except), paste0(range, ' ~ ', "'",EVENTOS,"'"),
                   paste0(range, " & ", exception, ' ~ ', "'" ,EVENTOS,"'"))
  ) %>%
  pull(code) %>%
  cat(sep = ',\n')


enos2 %>%
  filter(!str_detect(CODIGO,'-')) %>%
  mutate(
    CODIGO = str_replace(CODIGO, '\\(provisional\\)',''),
    code = paste0("str_detect(x, '", CODIGO, "') ~ '", EVENTOS, "'")) %>%
  pull(code) %>%
  cat(sep = ',\n')


tbl(con, dbplyr::in_schema('mortalidad', 'usudef16')) %>%
  collect %>%
  mutate(
    enos = code_enos(codmuer, edad, uniedad)
  ) %>%
  count(enos, grupedad) %>%
  arrange(desc(n), enos) %>%
  filter(str_detect(enos, 'MENIN')) %>%
  print(n = 300)



test_df %>%
  cie_check(edad, unieda, codmuer, sexo, juri) %>%
  cie_tbl_warnings() %>%
  View()


rec_age2day(as.character(deistools::test_df$edad), deistools::test_df$unieda)


#check_enos

library(tidyverse)
a <-  deistools::test_df %>%
  cie_check(edad, unieda, codmuer, sexo, juri)


a$tbl_enos %>%
  filter(!enos == 'Not ENOs')
