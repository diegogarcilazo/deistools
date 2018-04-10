cie_tbl_errors <-  function (x, ...) UseMethod('cie_tbl_errors', x);

cie_tbl_warnings <-  function (x, ...) UseMethod('cie_tbl_warnings', x);

cie_tbl_all <- function(x,...) UseMethod('cie_tbl_all', x);

cie_tbl_enos <- function(x,...) UseMethod('cie_tbl_enos', x);

cie_summary <- function(x,...) UseMethod('cie_summary', x);

is.cie_check <- function(x, ...) inherits(x, "cie_check");

#'Function to check CIE code by range of age, Suspected maternal death,
#'Valid cause of death, unlikely cause of death and sex restriction.
#'@param db dataset. death.
#'@param age age.
#'@param code_age. 1 years. 2 months. 3 days. 4 hours.
#'@param code_cie10. Vars with code of 3 or 4 letters.
#'@param sex sex.
#'@param ... vars for identification.

cie_check <- function(db, age, code_age, code_cie10, sex, ...){

#Capture arguments ########################################################

  id <- dplyr::quos(...)
  age <- dplyr::enquo(age)
  code_age <- dplyr::enquo(code_age)
  code_cie10 <- substitute(code_cie10)
  sex <- dplyr::enquo(sex)
  by <- `names<-`('code', deparse(code_cie10))
  db_name <- deparse(substitute(db))

#Join tables and create logical vars.########################################
tbl_complete_ck <- db %>%

  dplyr::mutate(
      code_age_check = deistools::rec_age2day(as.numeric(!!age), !!code_age)
    ) %>%

  dplyr::left_join(deistools::cie10_check, by) %>%

  dplyr::mutate(
      age_out = !((code_age_check > days_age_lower) &
                    (code_age_check < days_age_upper)),
      sex_out = (sex_limited != !!sex),
      SMD_in = !is.na(SMD_description) &
        (!!sex) == 2 & ((!!code_age) == 1 & dplyr::between(!!age, 11, 49))) %>%

  dplyr::select(!!!id, !!code_cie10, entity, useless, no_cbd, asterisco,
                trivial, !!age, !!code_age, age_out, days_age_lower,
                days_age_upper, !!sex, sex_out, SMD_in) %>%

  dplyr::filter(age_out | useless %in% 1:5 | no_cbd | asterisco |
             trivial | sex_out | SMD_in) %>%
    dplyr::mutate(
      i_no_cbd = dplyr::if_else(no_cbd, 'Not Valid BCD', '???'),
      i_asterisco = dplyr::if_else(asterisco, 'Not accepted as BCD', '???'),
      i_age_out = dplyr::if_else(age_out, 'Out of age limits', '???'),
      i_sex_out = dplyr::if_else(sex_out, 'Sex restriction', '???'),
      i_error = paste(i_no_cbd, i_asterisco, i_age_out, i_sex_out, sep = ' '),
      i_error = stringr::str_remove_all(i_error, '\\?\\?\\?|NA'),
      i_error = stringr::str_trim(i_error),
      error = ifelse(i_error == "", NA_character_, i_error),
      i_useless = ifelse(useless %in% 1:5, 'Useless code', '???'),
      i_trivial = ifelse(trivial, 'Unlikely to cause death', '???'),
      i_SMD_in = ifelse(SMD_in, 'Suspected Maternal Death', '???'),
      i_warning = paste(i_useless, i_trivial, i_SMD_in, sep = ' '),
      i_warning = stringr::str_remove_all(i_warning, '\\?\\?\\?|NA'),
      i_warning = stringr::str_trim(i_warning),
      warning = ifelse(i_warning == "", NA_character_, i_warning)
    )  %>% dplyr::select(-dplyr::starts_with('i_'))


tbl_enos <- db %>%
    dplyr::mutate(
      enos = deistools::code_enos(!!code_cie10, !!age, !!code_age, !!sex)
    ) %>% filter(!enos == 'Not ENOs') %>%
  dplyr::select(!!!id, !!code_cie10, entity, !!age, !!code_age, !!sex, enos)

#############################################################################
#Create cie_check class
cie_check <- list(df = tbl_complete_ck,
                    n_rows = dim(db)[1],
                    db_name = db_name,
                    tbl_enos = tbl_enos)

  class(cie_check) <- 'cie_check'

  invisible(cie_check)
}


#'create table with errors
#'@param x object class cie_check
#'@return tibble.
cie_tbl_errors.cie_check <- function(x) {
  stopifnot(is.cie_check(x))
  x$df %>%
    filter(age_out | no_cbd | asterisco | sex_out) %>%
    select(-useless, -trivial, -SMD_in, -warning, -sex_out,
           -age_out, -no_cbd, -asterisco, -days_age_lower,
           -days_age_upper)}

#'create table with warnings
#'@param x object class cie_check
#'@return tibble.
cie_tbl_warnings.cie_check <- function(x) {
  stopifnot(is.cie_check(x))
  x$df %>%
    filter(useless %in% 1:5 | trivial | SMD_in) %>%
    select(-age_out, -trivial, -days_age_lower, -days_age_upper,
           -no_cbd, -asterisco, -sex_out, -SMD_in, -error)}


#'create table with errors and warnings.
#'@param x object class cie_check
#'@return tibble.
cie_tbl_all.cie_check <- function(x) {
  stopifnot(is.cie_check(x))
  x$df}


#' Create summary report on console
#' @param x object class cie_check
cie_summary.cie_check <- function(x) {
stopifnot(is.cie_check(x))
summary_1 <- x$df %>%
    dplyr::summarise(
      `Age limit` = sum(age_out, na.rm = T),
      `Asterisk code` = sum(asterisco, na.rm = T),
      Trivial = sum(trivial, na.rm = T),
      `No CBD` = sum(no_cbd, na.rm = T),
      Useless = sum(useless %in% 1:5, na.rm = T),
      `Limited to one sex` = sum(sex_out, na.rm = T),
      SMD = sum(SMD_in, na.rm = T)
    ) %>%
    as.data.frame() %>%
    `rownames<-`(.,'n') %>%
    rownames_to_column %>%
    gather(indicator, value, -rowname) %>%
    spread(rowname, value) %>%
    mutate(pct = round(n * 100/ x$n_rows,1))

cat("\nCheck summary")
cat("\n")
cat(strrep('-', 70))
cat("\n")
cat("Dataset: ", x$db_name , "\nn = ", x$n_rows)
cat("\nErrors = ",
    suppressMessages(pull(tally(x$df %>%
                                  filter(age_out | no_cbd | asterisco | sex_out)))))
cat("\nWarnings = ",
    suppressMessages(pull(tally(x$df %>%
                                  filter(useless %in% 1:5 | trivial | SMD_in)))))
cat("\n\n")
print(summary_1)
cat(strrep('-', 70))
cat('\n')
cat(
  "# Asterisk: are valid as additional codes but are not accepted as\n\t    a basic cause of death.",
  "# Trivial: conditions unlikely to cause death.",
  "# No CBD: It is not valid as a Basic Cause of Death.",
  "# Limited to one sex: Identifies restriction codes associated with\n\t     gender.",
  "# Age limit: Out of Age limit accepted.",
  "# SMD: Suspected Maternal Death.",
  sep = '\n')
}

#'create table with Notifiable infectous diseases.
#'@param x object class cie_check
#'@return tibble.
cie_tbl_enos.cie_check <- function(x) {
  stopifnot(is.cie_check(x))

  cat(strrep('-', 70))
  cat('\nNotifiable infectous diseases:\n')
  cat('n = ', dim(x$tbl_enos)[1])
  cat('\n')

  x$tbl_enos %>%
    count(enos) %>%
    print(n = Inf)

  x$tbl_enos

  }
