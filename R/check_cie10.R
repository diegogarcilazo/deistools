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

  deistools::cie10_check %>% rename(!!code_cie10 := code)


  db %>% mutate(
    code_age_check = rec_age2day(as.numeric(!!age), !!code_age)
  ) %>%
    left_join(cie10_check %>%
                rename(!!code_cie10 := code)) %>%
    mutate(
      age_out = !((code_age_check > days_age_lower) &
                    (code_age_check < days_age_upper)),
      sex_out = (sex_limited != !!sex)) %>%
    select(!!id, !!code_cie10, entity, !!age, !!code_age, age_out,
           days_age_lower, days_age_upper, useless, no_cbd, asterisco,
           trivial, sex_out, SMD_description) %>%
    filter(age_out | useless %in% 1:5 | no_cbd | asterisco |
             trivial | sex_out | !is.na(SMD_description))


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


