context("testing")

testthat::test_that("cie_check works",
          {
  testthat::expect_identical(
    test_df %>%
      cie_check(edad, unieda, codmuer, sexo, juri),
    test_output_cie_check
  )
          })


testthat::test_that("cie_tbl_all works",
                    {
                      testthat::expect_identical(
                        test_output %>%
                          cie_tbl_all(),
                        test_output_cie_tbl_all
                      )
                    })
