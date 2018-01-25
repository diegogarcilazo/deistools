#' Return a message if all columns are or not are present by compare intersection length.
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
