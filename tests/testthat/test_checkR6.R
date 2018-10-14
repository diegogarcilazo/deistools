context("checkR6")


testthat::test_that(
  "Create instance problem",
  testthat::expect_warning(
    checkR6_instance <- deistools::checkCie10$new(deistools::test_df,
                                       edad, unieda, codmuer, sexo, ocloc,id)
  )

)

testthat::test_that(
  "list_all methods problem",
  expect_is(checkR6_instance$list_all(), "data.frame")
)

testthat::test_that(
  "list_unknown methods problem",
  expect_is(checkR6_instance$list_unknown(), "data.frame")
)

testthat::test_that(
  "list_enos methods problem",
  expect_is(checkR6_instance$list_enos(), "data.frame")
)

testthat::test_that(
  "list_problems methods problem",
  expect_is(checkR6_instance$list_problems(), "data.frame")
)


testthat::test_that(
  "plot_useless methods problem",
  expect_is(checkR6_instance$plot_useless(), "ggplot")
)


testthat::test_that(
  "plot_missing methods problem",
  expect_is(checkR6_instance$plot_missing(), "ggplot")
)


testthat::test_that(
  "help_useless methods problem",
  testthat::expect_is(checkR6_instance$help_useless(), "character")
)


testthat::test_that(
  "help_methods methods problem",
  testthat::expect_is(checkR6_instance$help_methods(), "character")
)


testthat::test_that(
  "help_indicators methods problem",
  testthat::expect_is(checkR6_instance$help_indicators(), "character")
)


testthat::test_that(
  "report_useless() methods problem",
  testthat::expect_known_output(checkR6_instance$report_useless(),
                               file = "report_useless_output.txt")
)


testthat::test_that(
  "report_useless() methods problem",
  testthat::expect_is(checkR6_instance$report_useless(), "glue")
)

testthat::test_that(
  "report_enos() methods problem",
  testthat::expect_is(checkR6_instance$report_enos(), "glue")
)


testthat::test_that(
  "completeness function",
  testthat::expect_length(
    completeness_tbl("sexo", deistools::test_df$sexo, c(1,2,3)),5)
)
