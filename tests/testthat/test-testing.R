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


library(tidyverse)
library(deistools)

con = pgr::pg_con(mdb1252, driver = PostgreSQL)

mort16 <- as_tibble(pgr::pg_sql(con, "SELECT edad,uniedad,sexo::INT,codmuer,juri FROM mortalidad.usudef16"))

a <- cie_check(mort16, edad, uniedad, codmuer, sexo, juri)

