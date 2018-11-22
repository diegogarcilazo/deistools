
checkCie10 <- R6::R6Class(
  "checkCie10",
  public = list(

    cats = NULL,

    initialize = function(db, age, code_age, code_cie10, sex, code_ocloc, ...) {

      private$names <- list(
        sex = deparse(substitute(sex)),
        age = deparse(substitute(age)),
        code_age = deparse(substitute(code_age)),
        code_cie10 = deparse(substitute(code_cie10)),
        code_ocloc = deparse(substitute(code_ocloc)))

      private$db <- db
      private$id <- dplyr::quos(...)
      private$age <- dplyr::enquo(age)
      private$code_age <- dplyr::enquo(code_age)
      private$code_cie10 <- substitute(code_cie10)
      private$code_cie10q <- dplyr::enquo(code_cie10)
      private$sex <- dplyr::enquo(sex)
      private$code_ocloc <- dplyr::enquo(code_ocloc)
      private$by <- `names<-`('code', deparse(private$code_cie10))
      private$db_name <- deparse(substitute(db))

      private$tbls <- private$db %>%
        dplyr::mutate(
          code_age_check = deistools::rec_age2day(
            as.numeric(!!private$age), !!private$code_age) #Age codeAge to days
        ) %>%
        dplyr::left_join(deistools:::cie10_check, private$by) %>% #Join db with cie10_check table.
        dplyr::mutate(
          age_out = !((code_age_check > days_age_lower) & (code_age_check < days_age_upper)), #Boolean result from days check
          sex_out = (sex_limited != !!private$sex), #Boolean result check sex limited.
          SMD_in = !is.na(SMD_description) & (!!private$sex) == 2 & ((!!private$code_age) == 1 & dplyr::between(!!private$age, 11, 49)))

      self$help_methods()

      return(self)

      },

    help_useless = function(){
      print(glue::glue(explain$help_useless))},

    help_indicators = function(){
      print(glue::glue(explain$help_indicators))},

    help_methods = function(){
      print(glue::glue(explain$help_methods))},

    help_place = function(){
      deistools::lkup_def_deis$OCLOC %>%
        glue::glue_data("{CODIGOS}: {DESCRIPCION}")},

    list_unknown = function(){

      result <- private$tbls %>%
        dplyr::filter(
          !(!!private$sex %in% 1:2) |
          !(!!private$code_age %in% 1:5) |
          !(!!private$age %in% 1:120),
          !is.na(entity)) %>%
        dplyr::select(!!!private$id, !!private$age, !!private$code_age,
               !!private$code_cie10, !!private$sex)
      return(result)
    },

    list_problems = function() {

      result <- private$tbls %>%
      dplyr::filter(useless %in% 1:5 | trivial | SMD_in |
               age_out | no_cbd | asterisco | sex_out) %>%
        dplyr::select(!!!private$id, !!private$age, !!private$code_age,
               !!private$code_cie10, entity,!!private$sex,
               useless, trivial, SMD_in, age_out,
               no_cbd, asterisco, sex_out) %>%
        dplyr::mutate_at(dplyr::vars(trivial, SMD_in, age_out,
                       no_cbd, asterisco, sex_out), dplyr::if_else, 1, 0) %>%
        tidyr::replace_na(list(age_out = 0, sex_out = 0))
      return(result)

    },

    list_useless = function() {

      result <- private$tbls %>%
        dplyr::filter(useless %in% 1:5) %>%
        dplyr::select(!!!private$id, !!private$age, !!private$code_age,
                      !!private$code_cie10, entity,!!private$sex,
                      useless)
      return(result)

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
      return(result)

    },

    list_all = function() {
      return(private$tbls)
    },

  plot_missing = function(){
    private$db %>%
      dplyr::select(!!private$age, !!private$code_age,
             !!private$code_cie10, !!private$sex) %>%
      dplyr::mutate(id = dplyr::row_number()) %>%
      dplyr::mutate_at(1:4, is.na) %>%
      tidyr::gather(var, miss, 1:4) %>%
      ggplot2::ggplot(ggplot2::aes(id, var, fill = miss)) +
      ggplot2::geom_raster() +
      ggplot2::theme_classic() +
      ggplot2::scale_fill_manual(
        values = c("TRUE" = "white", "FALSE" = "#5878F7")) +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_blank(),
            axis.line = ggplot2::element_blank(),
            legend.position = "none")

  },

  plot_useless = function(){
    private$tbls %>%
      dplyr::count(age = deistools::age_factor(code_age_check),
            useless = dplyr::if_else(useless == 0, "No","Sí"),
            age = dplyr::case_when(
              stringr::str_detect(age, "M1|M2") ~ "Neo",
              age == "M3" ~ "PosNeo",
              (age >= "01" & age <= "04") ~ "01 - 04",
              T ~ as.character(age)),
            age = forcats::fct_relevel(age, "Neo", "PosNeo")) %>%
      dplyr::group_by(age) %>%
      dplyr::mutate(prop = n/sum(n)) %>%
      dplyr::filter(useless == "Sí") %>%
      ggplot2::ggplot(ggplot2::aes(age, prop)) +
      ggplot2::geom_col(fill = "firebrick") +
      ggplot2::geom_text(ggplot2::aes(
        label = glue::glue("{scales::percent(prop, 1)}\n({n})")),
                size = 3, nudge_y = .06) +
      ggplot2::theme_classic() +
      ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
      ggplot2::theme(axis.title = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_blank(),
            axis.line = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_text(angle = 45))



  },


report_enos = function(){

  report <- list(
    report_01 = self$list_enos() %>%
      dplyr::count(
        enos = stringr::str_to_title(enos)) %>%
      dplyr::mutate(
        pct = round(n * 100 / sum(n), 1)
      ) %>%
      dplyr::arrange(desc(n)) %>%
      glue::glue_data('{seq_along(enos)}: {enos} [{n}, {pct}%]'))

     glue::glue(
"
Report Notifiable Infectous Diseases: [n, %]
--------------------------------------------

{glue::glue_collapse(report_01, sep = '\n')}
", .envir = report)

},

  report_useless = function(){

    report_data <- list(

      report_1 = private$tbls %>%
        dplyr::count(useless = dplyr::if_else(useless == 0, 'No','Sí')) %>%
        glue::glue_data(tbls_formats$report_1),

      report_2 = private$tbls %>%
        dplyr::filter(useless > 0) %>%
        dplyr::mutate(
          useless = factor(useless, levels = 1:5)) %>%
        dplyr::count(useless) %>%
        tidyr::complete(useless, fill = list(n = 0)) %>%
        dplyr::mutate(prop = n / sum(n)) %>%
        glue::glue_data(tbls_formats$report_2),

      report_3 = private$tbls %>%
        dplyr::count(age = deistools::age_factor(code_age_check),
              #useless = dplyr::if_else(useless == 0, "No","Sí"),
              useless = factor(useless, levels = 0:5),
              age = dplyr::case_when(
                stringr::str_detect(age, "M1|M2") ~ "Neo",
                age == "M3" ~ "PosNeo",
                (age >= "01" & age <= "04") ~ "01 - 04",
                T ~ as.character(age)),
              age = forcats::fct_relevel(age, "Neo", "PosNeo")) %>%
        tidyr::complete(useless, age) %>%
        tidyr::spread(useless, n, fill = 0) %>%
        glue::glue_data(tbls_formats$report_3),

      report_4 = private$tbls %>%
        dplyr::count(ocloc = !!private$code_ocloc,
                     useless = factor(useless, levels = 0:5)) %>%
        tidyr::complete(useless) %>%
        tidyr::spread(useless, n, fill = 0) %>%
        dplyr::mutate(prop = round(rowSums(.[,3:7]) * 100 /rowSums(.[,2:7]), 1)) %>%
        glue::glue_data(tbls_formats$report_4),

      report_5 = private$tbls %>%
        dplyr::count(sex = !!private$sex,
                     useless = factor(useless, levels = 0:5)) %>%
        tidyr::complete(useless) %>%
        tidyr::spread(useless, n, fill = 0) %>%
        dplyr::mutate(prop = round(rowSums(.[,3:7]) * 100 /rowSums(.[,2:7]), 1)) %>%
        glue::glue_data(tbls_formats$report_5)
    )

    glue::glue(explain$report_useless, .envir = report_data)
  },


class = list("sex" = is.integer,
              "age" = is.integer,
              "code_age" = is.integer,
              "code_cie10" = is.character,
              "code_ocloc" = is.integer),


report_completeness = function(){

  self$cats <- list(
    "sex" = c(1,2),
    "age" = c(1:120),
    "code_age" = c(1:5),
    "code_cie10" = deistools::cie10_cats,
    "code_ocloc" = c(1:4,9))

  purrr::pmap_df(
    list(private$names, private$vars(), self$cats),
    deistools::completeness_tbl)
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
    code_ocloc = NULL,
    by = NULL,
    db_name = NULL,
    db = NULL,
    tbls = NULL,
    names = NULL,
    vars = function() {
      private$tbls %>%
        dplyr::select(!!private$sex,
                      !!private$age,
                      !!private$code_age,
                      !!private$code_cie10,
                      !!private$code_ocloc)}

)
)
