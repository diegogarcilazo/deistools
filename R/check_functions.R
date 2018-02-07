#' Return a message if all columns are or not are present by compare intersection length.
#' Works for DEIS datasets.
#' @param tbl_1 tbl for check.
#' @param vital_fact vital fact of dataset. Options: 'deaths' or 'births'. Default 'deaths.

col_length <- function(tbl_1, vital_fact = 'deaths'){

  tbl_1_names <- names(tbl_1)

  stopifnot(vital_fact%in%c('deaths', 'births'));

  switch(vital_fact,
         'deaths' = {tbl_2_names <- deistools::lkup_def_deis$VARS$CAMPO},
         'births' = {tbl_2_names <- deistools::lkup_nv_deis$VARS$CAMPO})

  value = length(intersect(tbl_1_names, tbl_2_names)) == length(tbl_2_names)

  if(value){message('All columns are Ok.')} else {message('Warning. Columns are left.')}
}


#'Function to check cie10 code by
#'@param db dataset. death.
#'@param id unique identification of registry.
#'@param age age.
#'@param code_age. 1 years. 2 months. 3 days. 4 hours.
#'@param code_cie10. Vars with code of 3 or 4 letters.
#'@param sex sex.

check_cie10 <- function(db, id, age, code_age, code_cie10, sex){

  id <- enquo(id)
  age <- enquo(age)
  code_age <- enquo(code_age)
  code_cie10 <- substitute(code_cie10)
  sex <- enquo(sex)

  join <- db %>% mutate(
      code_age_check = rec_age2day(as.numeric(!!age), !!code_age)
    ) %>%
    left_join(cie10_check %>% rename(!!code_cie10 := code))


#complete tbl

tbl_complete_ck <- join %>%
    mutate(
      age_out = !((code_age_check > days_age_lower) &
                    (code_age_check < days_age_upper)),
      sex_out = (sex_limited != !!sex)) %>%
    select(!!id, !!code_cie10, entity, !!age, !!code_age, age_out,
           days_age_lower, days_age_upper, useless, no_cbd, asterisco,
           trivial, sex_out, SMD_description) %>%
    filter(age_out | useless %in% 1:5 | no_cbd | asterisco |
             trivial | sex_out | suspected_maternal_death %in% 1:55)


# Asterisco:	Código de asterisco, son validos como códigos adicionales,
#  no se aceptan como causa básica (F = No asterisco, T = Si asterisco)


# Trivial	Afecciones poco probables de provocar la muerte F = No trivial, T = Trivial.


# No CBD	No es válida como Causa Básica de Defunción
#  F = Si es causa válida, T = No es causa válida


# Lmitada a un sexo	Identifica la restricción de códigos asociadas al sexo
#  1 = masculino, 2 = femenino.

# Límite Inferior de edad	Límite de edad inferior aceptado (sugerido) (H = horas, D = Días, M = Meses, A = Años)
# Límite Superior de edad	Límite de edad superior aceptado (sugerido) (D = Días, M = Meses, A = Años)


return(
  tbl_complete_ck
  )

}





compare_class <- function(c){

  list_class <- list(
    atenmed = deistools::lkup_def_deis$ATENMED$CODIGOS,
    medsus = deistools::lkup_def_deis$MEDSUS$CODIGOS,
    codmuer = deistools::tbl_cie10$code,
    sexo = deistools::lkup_def_deis$SEXO$CODIGOS,
    ocloc = deistools::lkup_def_deis$OCLOC$CODIGOS,
    depoc = deistools::lkup_def_deis$Depart$CodDep,
    provoc = deistools::lkup_def_deis$JURI$CODIGOS,
    depres = deistools::lkup_def_deis$Depart$CodDep,
    provres = deistools::lkup_def_deis$JURI$CODIGOS,
    paisres = deistools::lkup_def_deis$`País`$CodPais,
    asociad = deistools::lkup_def_deis$ASOCIAD$CODIGOS,
    finstruc = deistools::lkup_def_deis$INSTRUC$CODIGOS,
    fsitlabor = deistools::lkup_def_deis$SITLABOR$CODIGOS,
    minstruc = deistools::lkup_def_deis$INSTRUC$CODIGOS,
    msitcony = deistools::lkup_def_deis$MSITCONY$CODIGOS,
    pinstruc = deistools::lkup_def_deis$INSTRUC$CODIGOS,
    sitlabor = deistools::lkup_def_deis$SITLABOR$CODIGOS)

  var_name <- deparse(substitute(c))

  typeof_c <- typeof(c)
  typeof_c_standard <- typeof(list_class)

  classes_c <- sort(unique(c))
  classes_standard <- sort(unique(list_class))


  classes_freq <- table(c) %>%
    sort(decreasing = T) %>%
    head(n = 5)

  outer_c <- classes_c[!classes_c%in%c(classes_standard)]
  inner_c <- classes_c[classes_c%in%c(classes_standard)]
  left <- classes_standard[!classes_standard%in%c(classes_c)]

  cat('Variable name: ', var_name,
      '\n--------------------------------------------------------------------',
      '\nType Of = Variable: ', typeof_c, ' -> Expected: ', typeof_c_standard,
      '\n--------------------------------------------------------------------')
  cat('\nClasses:' ,'\nIntersect \n')
  if(length(inner_c)>=10){
    cat(paste('->', head(inner_c,10)), sep = '\n')
    cat('....trucated in 10. Left ',length(inner_c)-10,' classes. See inner_c\n')
  }else{
    cat(paste('->', inner_c), sep = '\n')
  }
  cat('\nOutersect\n')
  if(length(outer_c)>=10){
    cat(paste('->', head(outer_c,10)), sep = '\n')
    cat('....trucated in 10. Left ',length(outer_c)-10,' classes. See outer_c\n')
  }else{
    cat(paste('->', outer_c), sep = '\n')
  }
  cat('\nLeft \n')
  if(length(left)>=10){
    cat(paste('->', head(left,10)), sep = '\n')
    cat('....trucated in 10. Left ',length(left)-10,' classes. See left\n')
  }else{
    cat(paste('->', left), sep = '\n')
  }

  list_classes <- list(outer_c = outer_c, inner_c = inner_c, left = left)
  invisible(list_classes)
}



