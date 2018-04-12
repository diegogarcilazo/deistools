library(tidyverse)


devtools::use_data(list_tbls, tbl_critred, tbl_cie10, tbl, cie10_check, test_df,
                   lookup_discharge, lkup_def_deis, lkup_nv_deis, lkuptbls_old,
                   test_output_cie_check, test_output_cie_tbl_all, internal = T,
                   overwrite = T)

# Function that link to database download from
# # http://www.paho.org/hq/index.php?option=com_docman&task=doc_download&gid=23700&Itemid=270&lang=en

#--------------------------------------------------------------------------------
update_cie10 <- function() {
  tbl <- readxl::read_excel('data/Volumen1CatalogoCIE-10.xls',
                            sheet = 'CatalogoVolumen1', range = 'B5:Y14423') %>%
    dplyr::transmute(code = Clave, entity = Nombre, chapter = `Capítulo`,
                  asterisco = Asterisco,
                  trivial = Trivial,
                  useless = `Lista causas poco útiles`,
                  no_cbd = `No_Causa Básica Defuncion`,
                  suspected_maternal_death = `Lista causa sospechosas de encubrir muerte materna`,
                  fetal = Fetal,
                  sex_limited = `Limitada a un sexo`,
                  age_lower = `Límite Inferior de edad`,
                  age_upper = `Límite Superior de edad`) %>%
    dplyr::mutate_if(is.character,
                     function(x) toupper(myutilities::acc_rm(x))
    )
  return(tbl)
}



suspected_maternal_death <- readxl::read_excel(
  'data/Listas_Asociadas-Catalogo.xls',
  sheet = 'Lista de causas sospechosas', range = 'A3:C58') %>%
  dplyr::transmute(code = as.integer(N.), SMD_description = Descripción, cie10 = `Códigos CIE-10`) %>%
  dplyr::mutate_if(is.character, ~ toupper(myutilities::acc_rm(.x)))


tbl_cie10 <- update_cie10()


#Crea tabla para chequeos

cie10_check <- tbl_cie10 %>%
  transmute(
    code,
    entity,
    asterisco = if_else(asterisco == 'T', T, F),
    trivial = if_else(trivial == 'T', T, F),
    no_cbd = if_else(no_cbd == 'T', T, F),
    useless = as.character(useless),
    fetal = if_else(fetal == 'T', T, F),
    suspected_maternal_death = as.integer(suspected_maternal_death),
    sex_limited,
    code_age_upper = str_replace_all(age_upper, '[^A-Z]', ''),
    code_age_lower = str_replace_all(age_lower, '[^A-Z]', ''),
    code_age_upper = case_when(
      code_age_upper == 'H' ~ 4,
      code_age_upper == 'D' ~ 3,
      code_age_upper == 'M' ~ 2,
      code_age_upper == 'A' ~ 1,
      T ~ NaN),
    code_age_lower = case_when(
      code_age_lower == 'H' ~ 4,
      code_age_lower == 'D' ~ 3,
      code_age_lower == 'M' ~ 2,
      code_age_lower == 'A' ~ 1,
      T ~ NaN),
    age_upper = as.numeric(str_replace_all(age_upper, '[A-Z]', '')),
    age_lower = as.numeric(str_replace_all(age_lower, '[A-Z]', '')),
    days_age_upper = rec_age2day(age_upper, code_age_upper),
    days_age_lower = rec_age2day(age_lower, code_age_lower)) %>%
  left_join(suspected_maternal_death, c('suspected_maternal_death' = 'code'))


#-------------------------------------------------------------------------------
con <- pgr::pg_con(mdb1252)

codegresp <- as_tibble(
  pgr::pg_sql(con,
              "SET CLIENT_ENCODING = 'UTF8'; SELECT * FROM eh_er.egreso")) %>%
  rename(codegresp = codegreso)

asociado <- as_tibble(
  pgr::pg_sql(con,
              "SET CLIENT_ENCODING = 'UTF8'; SELECT * FROM eh_er.asociado")) %>%
  mutate(
    asociado = myutilities::acc_rm(asociado)
  )

terminac <- readxl::read_xlsx('data/TERMINAC.xlsx')

establec <- as_tibble(
  pgr::pg_sql(con,
              "SET CLIENT_ENCODING = 'UTF8'; SELECT DISTINCT * FROM eh_er.establec")) %>%
  transmute(
    codest = stringr::str_pad(codest, 8, 'left', '0'),
    est_deninst = deninst,
    est_coddepart = stringr::str_pad(coddepart, 3, 'left', '0'),
    est_codprov = stringr::str_pad(codprov, 2, 'left', '0'),
    est_localidad = localidad
  ) %>%
  mutate_at(c(2,5),
    myutilities::acc_rm
  )

uoperat <- as_tibble(
  pgr::pg_sql(con,
              "SET CLIENT_ENCODING = 'UTF8'; SELECT DISTINCT * FROM eh_er.uoperat")) %>%
  transmute(
    uoperativa = stringr::str_trim(uoperativa),
    coduoperat = stringr::str_pad(coduoperat, 3, 'left', '0')
  ) %>%
  mutate_at(1,
            function(x) toupper(myutilities::acc_rm(x))
  )


causextl <- as_tibble(
  pgr::pg_sql(con,
              "SET CLIENT_ENCODING = 'UTF8'; SELECT DISTINCT * FROM eh_er.causextl")) %>%
  mutate(codcauextl = as.character(codcauextl))%>%
  mutate_at(2,
            function(x) toupper(myutilities::acc_rm(x))
  )

causextt <- as_tibble(
  pgr::pg_sql(con,
              "SET CLIENT_ENCODING = 'UTF8'; SELECT DISTINCT * FROM eh_er.causextt")) %>%
  mutate(codcauextt = as.character(codcauextt))%>%
  mutate_at(2,
            function(x) toupper(myutilities::acc_rm(x))
  )

clasienf <- as_tibble(
  pgr::pg_sql(con,
              "SET CLIENT_ENCODING = 'UTF8'; SELECT DISTINCT * FROM eh_er.clasienf")) %>%
  mutate(coddiagpr = codclasenf) %>%
  mutate_all(
            function(x) toupper(myutilities::acc_rm(x))
  )


tbl_cie10<- biotools::tbl_cie10

lookup_discharge <- list(asociado, causextl, causextt, clasienf, codegresp, establec, uoperat)

names(lookup_discharge) <- c('asociado', 'causextl', 'causextt', 'clasienf', 'codegresp', 'establec', 'uoperat')


# CRITERIOS DE REDUCIBILIDAD TABLAS
# 2010 y 2011 usar tabla tbl_critred.tsv, lo único diferente es un caso en el 2011.
# 2012 a 2015 usar tabla critred15b.tsv.
# Chequeado con anuarios de nación el 02/11/2017


#Leer las tablas que se encuentran en el paquete
critred1011 <- read_delim('data/tbl_critred.tsv', delim = '\t') %>% rename(critred = critred1011)
critred1215 <- read_delim('data/critred15b.tsv', delim = '\t') %>% rename(critred = critred15)


#Unir las tablas y asignar el code_redu que combina las variables. El code_redu utiliza los 4 primeros
#digitos para definir el intervalo de años, ejemplo: 1215 significa tabla para el período 2012 - 2015.
#el 5 dígito es 1 para las neonatales y 2 para las posneonatales.
#Esta table se debe usar junto con la función code_redu.

tbl_critred <- bind_rows(
  critred1215 %>%
    transmute(
      codmuer,
      code_redu = case_when(grupedad == 'NEONATAL' ~ 12151L,
                            grupedad == 'POSNEONATAL' ~ 12152L,
                            T ~ NA_integer_),
      critred),
  critred1011 %>%
    transmute(
      codmuer,
      code_redu = case_when(grupedad == 'NEONATAL' ~ 10111L,
                            grupedad == 'POSNEONATAL' ~ 10112L,
                            T ~ NA_integer_),
      critred),
  data_frame(codmuer = 'R99x', code_redu = 12152L, critred = 'MAL DEFINIDAS')) %>%
  transmute(code_redu = paste0(codmuer, code_redu), critred)
