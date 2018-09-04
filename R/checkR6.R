require(tidyverse)

checkCie10 <- R6::R6Class(
  "checkCie10",
  public = list(
    initialize = function(db, age, code_age, code_cie10, sex, ...) {
      private$db <- db
      private$id <- dplyr::quos(...)
      private$age <- dplyr::enquo(age)
      private$code_age <- dplyr::enquo(code_age)
      private$code_cie10 <- substitute(code_cie10)
      private$code_cie10q <- enquo(code_cie10)
      private$sex <- dplyr::enquo(sex)
      private$by <- `names<-`('code', deparse(private$code_cie10))
      private$db_name <- deparse(substitute(db))
      private$tbls <- private$db %>%
        dplyr::mutate(
          code_age_check = deistools::rec_age2day(
            as.numeric(!!private$age), !!private$code_age) #Age codeAge to days
        ) %>%
        dplyr::left_join(deistools::cie10_check, private$by) %>% #Join db with cie10_check table.
        dplyr::mutate(
          age_out = !((code_age_check > days_age_lower) & (code_age_check < days_age_upper)), #Boolean result from days check
          sex_out = (sex_limited != !!private$sex), #Boolean result check sex limited.
          SMD_in = !is.na(SMD_description) & (!!private$sex) == 2 & ((!!private$code_age) == 1 & dplyr::between(!!private$age, 11, 49)))

      self$help()

      },

    help = function(){
      cat(
        glue::glue(private$console_1))},

    list_unknown = function(){

      private$tbls %>%
        filter(
          !(!!private$sex %in% 1:2) |
          !(!!private$code_age %in% 1:5) |
          !(!!private$age %in% 1:120),
          !is.na(entity)) %>%
        select(!!!private$id, !!private$age, !!private$code_age,
               !!private$code_cie10, !!private$sex)
      invisible(self)
    },

    list_problems = function() {

      result <- private$tbls %>%
      filter(useless %in% 1:5 | trivial | SMD_in |
               age_out | no_cbd | asterisco | sex_out) %>%
        select(!!!private$id, !!private$age, !!private$code_age,
               !!private$code_cie10, entity,!!private$sex,
               useless, trivial, SMD_in, age_out,
               no_cbd, asterisco, sex_out) %>%
        mutate_at(vars(trivial, SMD_in, age_out,
                       no_cbd, asterisco, sex_out), if_else, 1, 0) %>%
        replace_na(list(age_out = 0, sex_out = 0))
      invisible(result)

    },

    list_enos = function() {
      result <- private$tbls %>%
        dplyr::transmute(
          !!!private$id, !!private$code_cie10, entity,!!private$age,
          !!private$code_age, !!private$sex,
          enos = deistools::code_enos(!!private$code_cie10,
                                      !!private$age,
                                      !!private$code_age,
                                      !!private$sex)
        ) %>% dplyr::filter(!enos == 'No ENOs')
      invisible(result)

    },

    list_all = function() {
      invisible(private$tbls)
    },

  summary = function(){
    private$db %>%
    select(!!!private$id, !!private$age, !!private$code_age,
           !!private$code_cie10,!!private$sex) %>%
      skimr::skim()
  },

  plot_missing = function(){
    private$db %>%
      select(!!private$age, !!private$code_age,
             !!private$code_cie10, !!private$sex) %>%
      mutate(id = row_number()) %>%
      mutate_at(1:4, is.na) %>%
      gather(var, miss, 1:4) %>%
      ggplot(aes(id, var, fill = miss)) + geom_raster() +
      theme_classic() +
      scale_fill_manual(values = c("TRUE" = "white", "FALSE" = "#5878F7")) +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_blank(),
            legend.position = "none")

  },

  plot_useless = function(){
    private$tbls %>%
      count(age = deistools::age_factor(code_age_check),
            useless = if_else(useless == 0, "No","Sí"),
            age = case_when(
              str_detect(age, "M1|M2") ~ "Neo",
              age == "M3" ~ "PosNeo",
              (age >= "01" & age <= "04") ~ "01 - 04",
              T ~ as.character(age)),
            age = fct_relevel(age, "Neo", "PosNeo")) %>%
      group_by(age) %>%
      mutate(prop = n/sum(n)) %>%
      filter(useless == "Sí") %>%
      ggplot(aes(age, prop)) +
      geom_col(fill = "firebrick") +
      geom_text(aes(label = glue::glue("{scales::percent(prop, 1)}({n})")),
                size = 3, nudge_y = .02) +
      theme_classic() +
      scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
      theme(axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_blank(),
            axis.text.x = element_text(angle = 45))



  }

),

  private = list(
    #vars list
    id = NULL,
    age = NULL,
    code_age = NULL,
    code_cie10 = NULL,
    code_cie10q = NULL,
    sex = NULL,
    by = NULL,
    db_name = NULL,
    db = NULL,
    tbls = NULL,
    console_1 = "
------------------------------------------------------------------
Dataset = {private$db_name}
rows = {dim(private$tbls)[1]}

------------------------------------------------------------------
Indicators:
------------------------------------------------------------------
Errors:
  1. Age limit: Out of Age limit accepted.
  2. Asterisk: are valid only as additional codes.
  3. Limited to one sex: Restriction codes associated with gender.
  4. No CBD: It is not valid as a Basic Cause of Death.

Warnings:
  5. SMD: Suspected Maternal Death.
  6. Trivial: conditions unlikely to cause death.
  7. Useless Codes

------------------------------------------------------------------
list methods:
------------------------------------------------------------------
  list_problems(). To list certificates problems.
  list_enos(). Check for Notifiable infectous diseases.
  list_unknown(). Check for unknown categories.
  list_all(). list all vars.

------------------------------------------------------------------
plot methods:
------------------------------------------------------------------
  plot_missing(). raster by missing status.
  plot_useless(). Plot % of useless code by group age.
------------------------------------------------------------------

"
  )
      )

