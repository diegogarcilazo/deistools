#' Return a message if all columns are or not are present by compare
#' intersection length. Works for DEIS datasets.
#' @param tbl_1 tbl for check.
#' @param vital_fact vital fact of dataset. Options: 'deaths' or 'births'.
#' Default 'deaths.


col_length <- function(tbl_1, vital_fact = 'deaths'){

  tbl_1_names <- names(tbl_1)

  stopifnot(vital_fact%in%c('deaths', 'births'));

  switch(vital_fact,
         'deaths' = {tbl_2_names <- deistools::lkup_def_deis$VARS$CAMPO},
         'births' = {tbl_2_names <- deistools::lkup_nv_deis$VARS$CAMPO})

  value = length(intersect(tbl_1_names, tbl_2_names)) == length(tbl_2_names)

  if(value){message('All columns are Ok.')} else {message('Warning. Columns are left.')}
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



