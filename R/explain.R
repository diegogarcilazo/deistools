explain <- list(

  help_useless =
    "
  Useless codes explanation:

  1. Causes that cannot or should not be considered as underlying
  causes of death.
  2. Intermediate causes of death such as heart failure,
  septicemia, peritonitis, osteomyelitis, or pulmonary embolism.
  3. Immediate causes of death that are the final steps in a
  disease pathway leading to death.
  4. Unspecified causes within a larger cause grouping.
  *Author: S. Makela, et al. 2010, Algorithms for enhancing
  public health utility of national causes-of-death data
  5. Ill-defined conditions.",


  help_indicators = "
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
  ",

  help_methods = "
  ------------------------------------------------------------------
  Dataset = {private$db_name}
  rows = {dim(private$tbls)[1]}
  ------------------------------------------------------------------
  list methods:

  list_useless(). List certificates with useless code.
  list_problems(). To list certificates problems.
  list_enos(). Check for Notifiable infectous diseases.
  list_unknown(). Check for unknown categories.
  list_all(). list all vars.

  ------------------------------------------------------------------
  Report methods
  ------------------------------------------------------------------
  report_useless()
  report_enos()
  report_unknown()

  ------------------------------------------------------------------
  plot methods:
  ------------------------------------------------------------------
  plot_missing(). raster by missing status.
  plot_useless(). Plot % of useless code by group age.

  ------------------------------------------------------------------
  Help methods
  ------------------------------------------------------------------
  help_useless()
  help_indicators()
  help_methods()
  help_place()

  ------------------------------------------------------------------
  ",

  report_useless = "

    Useless Report
    ----------------------------

    {report_1}


    1. Code Distribution:

    {glue::glue_collapse(report_2, sep = '\n    ')}


   2. Place of Occurrence:

    Local   | Code 0 | Code 1 | Code 2 | Code 3 | Code 4 | Code 5 |  %  |
    ---------------------------------------------------------------------
  {glue::glue_collapse(report_4, sep = '\n  ')}


    3. Age Distribution:

      Edad | Code 0 | Code 1 | Code 2 | Code 3 | Code 4 | Code 5 |  %  |
      ------------------------------------------------------------------
  {glue::glue_collapse(report_3, sep = '\n  ')}

  "
)


tbls_formats <- list(

report_1 = "
{useless[2]}: {n[2]} ({round(n[2] * 100 / sum(n),1)}%)
  {useless[1]}: {n[1]}
  Total: {sum(n)}",

report_2 = "
code {useless}:\\
{format(n, width = stringr::str_length(max(n)) + 1, justify = 'right')} \\
{format(round(prop*100,1), width = stringr::str_length(max(n)) + 1, justify = 'right')}%",

report_3 = "
{format(age, width = 9, justify = 'right')}\\
{format(`0`, width = 9, justify = 'centre')}\\
{format(`1`, width = 9, justify = 'centre')}\\
{format(`2`, width = 9, justify = 'centre')}\\
{format(`3`, width = 9, justify = 'centre')}\\
{format(`4`, width = 9, justify = 'centre')}\\
{format(`5`, width = 9, justify = 'centre')}\\
{format(round((`1`+`2`+`3`+`4`+`5`) * 100 / (`0`+`1`+`2`+`3`+`4`+`5`),1), width = 6, justify = 'centre')}",

report_4 = "
{format(ocloc, width = 5)}:\\
{format(`0`, width = 9)}\\
{format(`1`, width = 9)}\\
{format(`2`, width = 9)}\\
{format(`3`, width = 9)}\\
{format(`4`, width = 9)}\\
{format(`5`, width = 9)}\\
{format(prop, width = 9)}"
)


