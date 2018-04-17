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



#import package
library(tidyverse)
library(deistools)

#read excel
atotales <- readxl::read_excel('/home/diego/Descargas/AtencionesTotales2018.xlsx')

#data wrangling
atotales <- atotales %>%
  mutate(
    uniedad = if_else(EDAD == 0, 2, 1),
    sexo = case_when(SEXO == 'F' ~ 2, SEXO == 'M' ~ 1, T ~ NaN),
    diagnostico = case_when(DIAGNOSTICO == 'No cargado' ~ NA_character_, T ~ DIAGNOSTICO)
  ) %>%
  separate(diagnostico, c('cie10', 'diag'), sep = ' - ') %>%
  mutate(
    cie10 = str_remove(cie10, '\\.'),
    enos = code_enos(cie10, EDAD, uniedad, sexo)
  )

atotales %>%
  count(cie10,enos) %>%
  arrange(desc(n)) %>%
  write_tsv('/home/diego/Descargas/AtencionesTotales2018.tsv')



