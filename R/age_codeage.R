#'This is a helper function that transform age and age code to days.
#'@param age age. Integer.
#'@param code_age code age Assume code_age levels 1 = years, 2 = months,
#'3 = days, 4 = hours, 5 = minutes.

rec_age2day <- function(age,code_age) {

  age = as.numeric(age)

  if (any(age==0)) {warning('There are age equals 0 will be updated to 1')}

  age = if_else(age == 0, 1, age)

    dplyr::case_when(
      code_age == 3 ~ age,
      code_age == 2 ~ age * 30,
      code_age == 1 ~ age * 365.25,
      code_age == 4 ~ age / 24 / 30,
      code_age == 5 ~ age / 60 / 24 / 30,
      code_age == 9 ~ NaN,
      TRUE ~ NaN)

  }


#' recode age group from two variables age and code_age.
#' @param age num: numeric age.
#' @param code_age num: numeric code_age
#' @return ordered factor with age groups.


age_codeage <- function(age, code_age) {

adays <- deistools::rec_age2day(age,code_age)

breaks = c(0,6,27,330,365.25,730.5,1095.75,
					 1461, 3287.25, 5113.5, 6939.75, 8766, 10592.25, 12418.5, 14244.75,
					 16071, 17897.25, 19723.5, 21549.75, 23376, 25202.25, 27028.5,
					 28854.75, 30681, 43464.75)

labels = c('M1','M2','M3','01','02','03','04','05 - 09',
           '10 - 14','15 - 19','20 - 24','25 - 29',
					 '30 - 34','35 - 39','40 - 44','45 - 49','50 - 54',
					 '55 - 59','60 - 64','65 - 69','70 - 74',
					 '75 - 79','80 - 84','85 y +')

cut(adays, breaks, labels, ordered_result = T)}
