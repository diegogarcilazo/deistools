context("age_codeage")

test_that(
  "rec_age2day expect value 730.5",
  {expect_equal(rec_age2day(2, 1),730.5)}
)

test_that("rec_age2day expect warning",
{expect_warning(rec_age2day(0,1), 'There are age equals 0 will be coerced to 1')}
)

test_that(
  "age_codeage is a factor",
  {expect_is(age_codeage(2,1), "factor")}
)



context("code_redu")

test_that(
  "code_redu expect value 'J99910111'",
  {expect_equal(code_redu(2010, 'M1', 'J999'), "J99910111")}
)
