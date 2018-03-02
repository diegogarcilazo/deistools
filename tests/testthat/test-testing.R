context("testing")

testthat::test_that("cie_check works",
          {
  testthat::expect_identical(
    deistools::test_df %>%
      cie_check(edad, unieda, codmuer, sexo, id),
    deistools::test_output_cie_check
  )
          })

testthat::test_that("cie_tbl_all works",
                    {
                      testthat::expect_identical(
                        deistools::test_output_cie_check %>%
                          cie_tbl_all(),
                        deistools::test_output_cie_tbl_all
                      )
                    })
