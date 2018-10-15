library(pgr)
library(tidyverse)
library(deistools)

devtools::test()
devtools::check()
devtools::document()

devtools::install_github('diegogarcilazo/deistools')

devtools::use_package("dplyr")
devtools::use_package("R6")
devtools::use_package("glue")
devtools::use_package("crayon")
devtools::use_package("stringr")
devtools::use_package("forcats")
devtools::use_package("magrittr")
devtools::use_package("tibble")

devtools::use_test("testing")
devtools::use_testthat()

con <- pgr::pg_con(mdb1252)
remove.packages('deistools')
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


con <- pgr::pg_con(mdb1252)

pgr::pg_show(con, 'mortalidad')

usudef16 <- tbl(con, dbplyr::in_schema('mortalidad','usudef16')) %>% collect()


library(deistools)

a <- cie_check(usudef16, edad, uniedad, codmuer, sexo, juri)

cie_summary(a)


juris <- as.character(sort(unique(usudef16$juri)))

walk(juris, ~ {cat('\n\nJuris: ',.x); cie_check(filter(usudef16, juri == .x),
                 edad, uniedad, codmuer, sexo, depoc) %>% cie_summary} )



deistools::lkup_def_deis$JURI %>% print(n = 100)

library(tidyverse)

con <- pgr::pg_con(mdb1252, driver = PostgreSQL)

df <- as_tibble(
  pgr::pg_sql(con, 'SELECT ano,codmuer,edad,uniedad::INT,provres FROM mortalidad.i01_v24'))


l <- deistools::reductible(df, edad, uniedad, ano, codmuer, provres)

filter(l,provres == 30, ano%in%2010:2016) %>%
  count(gedad2,key1,ano) %>%
  ggplot(aes(ano, n, fill = key1)) + geom_bar(stat = 'identity') + facet_wrap(~gedad2)

map(2010:2016, ~{print(.x);redu_summary(filter(l, ano == .x))})


redu1 <- tibble(
  lab = sort(unique(deistools::tbl_critred$critred)[-2],decreasing = T),
  code = 1:13)


tbl_critred_labs <- mutate(redu1,
                           key1 = if_else(str_detect(lab,'^REDUCIBLE |OTRAS REDUCIBLES'),
                                          'Reducible',
                                          str_to_title(lab)),
                           key2 = case_when(
                             str_detect(lab, 'EN EL RECIEN NACIDO') ~ 'En el Recién Nacido',
                             str_detect(lab, 'EN EL PERIODO PERINATAL') ~ 'En el Período Perinatal',
                             str_detect(lab, 'EN EL PARTO') ~ 'En el Parto',
                             str_detect(lab, 'EN EL EMBARAZO') ~ 'En el Embarazo',
                             str_detect(lab, 'OTRAS REDUCIBLES|REDUCIBLE POR TRATAMIENTO|REDUCIBLE POR PREVENCION Y TRATAMIENTO|REDUCIBLE POR PREVENCION') ~ 'Otras Reducibles'),
                           key3 = case_when(
                             str_detect(lab, 'POR TRATAMIENTO QUIRURGICO') ~ 'Por Tratamiento Quirúrgico',
                             str_detect(lab, 'POR TRATAMIENTO CLINICO Y QUIRURGICO') ~ 'Por Tratamiento Clínico y Quirúrgico',
                             str_detect(lab, 'POR TRATAMIENTO CLINICO') ~ 'Por Tratamiento Clínico')

) %>% select(-1)


tbl_critred2 <- left_join(deistools::tbl_critred, redu1,c('critred'= 'lab')) %>%
  select(1,3)


tibble(
  key2 = 1:7,
  lab2 = drop_na(tbl_critred_labs,key2) %>% pull(3))


library(tidyverse)
library(deistools)

con <- pgr::pg_con(mdb1252, driver = PostgreSQL)

base <- pgr::pg_sql(con, "SELECT edad, uniedad::INT, codmuer, sexo, juri, ano FROM mortalidad.def0116")

glimpse(base)

chequeo <- cie_check(base, edad, uniedad, codmuer, sexo, juri, ano)


a <- cie_check(filter(base, ano == 2001), edad, uniedad, codmuer, sexo, juri, ano)


a <- map(2001:2016, ~cie_check(filter(base, ano == .x), edad, uniedad, codmuer, sexo, lococ,juri, ano)) %>%
  set_names(2001:2016)


lista <- map('2001':'2016', ~as.data.frame(a[[as.character(.x)]]$tbl_useless2) %>% rownames_to_column('GEDAD')) %>% set_names(2001:2016)

writexl::write_xlsx(lista, "/home/diego/Escritorio/useless2.xls")


rownames_to_column(lista$`2001`, 'gedad')




check <- function(db, age, code_age, code_cie10, sex, loc_oc, ...){

  id <- dplyr::quos(...)
  age <- dplyr::enquo(age)
  code_age <- dplyr::enquo(code_age)
  code_cie10 <- substitute(code_cie10)
  sex <- dplyr::enquo(sex)
  loc_oc <- dplyr::enquo(loc_oc)
  by <- `names<-`('code', deparse(code_cie10))
  db_name <- deparse(substitute(db))

  lista <- list(
    name = list(quo_name(sex), quo_name(age),
                quo_name(code_age), quo_name(code_cie10),
                quo_name(loc_oc)),
    var = list(rlang::eval_tidy(sex, db),
               rlang::eval_tidy(age, db),
               rlang::eval_tidy(code_age, db),
               rlang::eval_tidy(code_cie10, db),
               rlang::eval_tidy(loc_oc, db)),
    cats = list(
                c(1,2),
                c(1:120),
                c(1:5),
                deistools::tbl_cie10 %>%
                  filter(str_length(code) == 4) %>%
                  pull(code),
                c(1,2,3,4,9)),
    class = list(is.integer,
                 is.integer,
                 is.integer,
                 is.character,
                 is.integer)
  )

  pwalk(lista, ~ unknown_cats(..2, ..3))
}


check(deistools::test_df, edad, unieda, codmuer, sexo, ocloc)

















unknown_cats_vec <- function(var, cats){
  vec <- setdiff(unique(var), cats)
  return(vec)
}


are_unknown_cats <- function(var, cats){
  anyNA(match(unique(var), cats))
}

print_unknown_cats <- function(name, vec){
  str <- glue::glue_collapse(vec, sep = ", ", width = 50,
                             last = " and ")
  glue::glue("In var {name}. Unknown categories: {str}")

}


unknown_cats <- function(name, var, cats){

  if(are_unknown_cats(var, cats)){
    warning(
      print_unknown_cats(name, unknown_cats_vec(var, cats)),
      call. = F)
  }
}


pwalk(list(names, vars, cats), unknown_cats)

vec_check_class <- invoke_map_lgl(class, map(vars, list))

any(vec_check_class)

vec_check_class[!vec_check_class] %>% names

invoke_map_lgl(class, map(vars, list))

map2_lgl(deistools::test_df %>%
           select(sexo, edad, unieda, codmuer),
         cats,
         are_unknown_cats)


map_lgl(deistools::test_df %>%
          select(sexo, edad, unieda, codmuer),
        anyNA)




test_df <- deistools::test_df %>% mutate(ocloc = rep(c(1,2,3,4,9),200))


library(deistools)

checkR6_instance <- checkCie10$new(deistools::test_df,
                                   edad, unieda, codmuer, sexo, ocloc, id)



checkR6_instance$report_completeness()


