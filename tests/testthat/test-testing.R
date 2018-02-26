context("testing")

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})



testthat::test_that("check_cie10 works",
          {
  testthat::expect_identical(
    test_df %>%
      check_cie10(edad, unieda, codmuer, sexo, juri),
    test_output
  )
          })
