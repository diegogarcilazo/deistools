library(tidyverse)

# Function that link to database download from
# http://www.paho.org/hq/index.php?option=com_docman&task=doc_download&gid=23700&Itemid=270&lang=en

update_cie10 <- function() {
  tbl <- readxl::read_excel('data/Volumen1CatalogoCIE-10.xls',
                            sheet = 'CatalogoVolumen1', range = 'B5:Y14423') %>%
    dplyr::select(code = Clave, entity = Nombre, chapter = Capítulo,
                  useless = `Lista causas poco útiles`,
                  suspected_maternal_death = `Lista causa sospechosas de encubrir muerte materna`,
                  fetal = Fetal,
                  sex_limited = `Limitada a un sexo`,
                  age_lower = `Límite Inferior de edad`, age_upper = `Límite Superior de edad`) %>%
    dplyr::mutate_if(is.character,
                     function(x) toupper(myutilities::acc_rm(x))
    )
  return(tbl)
}

tbl_cie10 <- update_cie10()

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


devtools::use_data(list_tbls, tbl_critred, tbl_cie10, tbl,
                   lookup_discharge, lkuptbls_deis2016, lkuptbls_old,
                   internal = T, overwrite = T)

