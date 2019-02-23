deistools
================

Visión General
--------------

***deistools*** es un paquete que simplifica la realización de tareas habituales de las áreas de bioestadística.

Instalación
-----------

``` r
install.packages(c('devtools','tidyverse','readxl','stringr')) #Dependencias
devtools::install_github('diegogarcilazo/deistools') #Instalar paquete de github
```

Funciones de Chequeo
--------------------

El paquete deistools tiene incorporadas funciones para validar la información de las bases de mortalidad generadas por la DEIS.

El chequeo requiere información individual de las variables edad, codigo de edad, codigo de muerte (CIE10) y sexo. Estas variables son contrastadas con la información suministrada por la PAHO.

\* Fuente de información: Se utilizó la información de las tablas sugeridas por <http://www.paho.org/hq/index.php?option=com_docman&task=doc_download&gid=23700&Itemid=270&lang=en>

Chequeo de Códigos Poco Útiles
------------------------------

### Crear instancia

La `checkCie10$new(args)` crea el objeto de clase checkCie10 que nos permitirá utilizar los métodos de chequeo. La creación del objeto requiere de la indicación de variables obligatorias (edad, unidad de la edad, código de muerte, local de ocurrencia y sexo) y variables que consideremos importantes para la identificación de los registros.

``` r
library(deistools)

chequeo <- checkCie10$new(deistools::test_df, edad, unieda, codmuer, sexo, ocloc, id)
```

    Warning in deistools::rec_age2day(as.numeric(~edad), ~unieda): There are
    age equals 0 will be coerced to 1

    ------------------------------------------------------------------
    Dataset = deistools::test_df
    rows = 1000
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

Métodos de Códigos poco útiles.
-------------------------------

**report\_useless()**: Crea un reporte en la consola con 4 tablas que nos permiten evaluar el peso y la distribución de los códigos poco útiles.

``` r
chequeo$report_useless()
```

    Warning: Factor `age` contains implicit NA, consider using
    `forcats::fct_explicit_na`


      Useless Report
      ----------------------------

      Sí: 369 (36.9%)
      No: 631
      Total: 1000


      1. Code Distribution:

      code 1:  16  4.3%
      code 2: 220 59.6%
      code 3:  33  8.9%
      code 4:  37 10.0%
      code 5:  63 17.1%


     2. Place of Occurrence:

      Local   | Code 0 | Code 1 | Code 2 | Code 3 | Code 4 | Code 5 |  %  |
      ---------------------------------------------------------------------
        1:      117        4       45        6       12       16     41.5
        2:      127        4       45       11        5        8     36.5
        3:      130        4       39        4        8       15     35.0
        4:      116        0       55        8        8       13     42.0
        9:      141        4       36        4        4       11     29.5


      3. Age Distribution:

        Edad | Code 0 | Code 1 | Code 2 | Code 3 | Code 4 | Code 5 |  %  |
        ------------------------------------------------------------------
          Neo       27        0        1        0        0        0   3.6
       PosNeo        5        0        0        0        0        2  28.6
      01 - 04        4        0        2        0        0        0  33.3
      05 - 09        4        1        2        0        0        0  42.9
      10 - 14        1        0        0        0        0        0   0.0
      15 - 19        2        0        0        0        2        0  50.0
      20 - 24       12        0        0        0        1        0   7.7
      25 - 29        8        0        2        0        1        0  27.3
      30 - 34        9        0        0        1        2        1  30.8
      35 - 39       14        0        2        0        1        3  30.0
      40 - 44       12        0        1        2        1        1  29.4
      45 - 49       17        0        7        0        3        1  39.3
      50 - 54       23        0        4        1        2        3  30.3
      55 - 59       38        0        8        0        2        4  26.9
      60 - 64       53        2        7        4        3        3  26.4
      65 - 69       53        0       15        1        1        5  29.3
      70 - 74       67        1       32        3        5       10  43.2
      75 - 79       71        4       27        6        2       12  41.8
      80 - 84       83        2       37        5        6        8  41.1
       85 y +      127        6       71       10        5        9  44.3
           NA        1        0        2        0        0        1  75.0


      4. Sex Distribution:

        Sex | Code 0 | Code 1 | Code 2 | Code 3 | Code 4 | Code 5 |  %  |
        ------------------------------------------------------------------
        1:      349        9      110       17       25       39     36.4
        2:      279        7      110       16       12       24     37.7
        3:        1        0        0        0        0        0      0.0
        9:        2        0        0        0        0        0      0.0

**list\_useless**: Genera listas (data.frame) con los registros de códigos poco útiles.

``` r
chequeo$list_useless()
```

    # A tibble: 369 x 7
          id  edad unieda codmuer entity                           sexo useless
       <int> <int>  <int> <chr>   <chr>                           <int> <chr>  
     1     6    74      1 I509    INSUFICIENCIA CARDIACA, NO ESP…     2 2      
     2     6    88      1 I509    INSUFICIENCIA CARDIACA, NO ESP…     2 2      
     3     2    71      1 I509    INSUFICIENCIA CARDIACA, NO ESP…     1 2      
     4     6     7      1 G934    ENCEFALOPATIA NO ESPECIFICADA       2 2      
     5     6    85      1 I509    INSUFICIENCIA CARDIACA, NO ESP…     2 2      
     6    50    98      1 I10X    HIPERTENSION ESENCIAL (PRIMARI…     2 1      
     7     6    80      1 A419    SEPSIS, NO ESPECIFICADA             1 2      
     8    66    71      1 J81X    EDEMA PULMONAR                      1 2      
     9    82    49      1 R99X    OTRAS CAUSAS MAL DEFINIDAS Y L…     1 5      
    10     6    74      1 I709    ATEROSCLEROSIS GENERALIZADA Y …     1 1      
    # … with 359 more rows

Métodos de Códigos de Enfermedades de Notificación Obligatoria (ENOs).
----------------------------------------------------------------------

**report\_enos()**: Crea un reporte que nos permiten evaluar la distribución de los códigos correspondientes en a Enfermedades de Notificación Obligatoria (ENOs).

``` r
chequeo$report_enos()
```

    Report Notifiable Infectous Diseases: [n, %]
    --------------------------------------------

    1: Neumonía [49, 79%]
    2: Sida [5, 8.1%]
    3: Enfermedad De Chagas [4, 6.5%]
    4: Bronquiolitis < 2 [1, 1.6%]
    5: Diarreas Agudas [1, 1.6%]
    6: Meningitis Bacteriana Sin Especificar Agente [1, 1.6%]
    7: Tuberculosis [1, 1.6%]

**list\_enos**: Lista (data.frame) los certificados con códigos de enfermedades de notificación obligatoria.

``` r
chequeo$list_enos()
```

    # A tibble: 62 x 7
          id codmuer entity                           edad unieda  sexo enos   
       <int> <chr>   <chr>                           <int>  <int> <int> <chr>  
     1     6 J189    NEUMONIA, NO ESPECIFICADA          20      1     2 Neumon…
     2     2 B208    ENFERMEDAD POR VIH, RESULTANTE…    33      1     2 SIDA   
     3    14 J180    BRONCONEUMONIA, NO ESPECIFICADA    79      1     2 Neumon…
     4     6 J189    NEUMONIA, NO ESPECIFICADA          81      1     1 Neumon…
     5     6 J189    NEUMONIA, NO ESPECIFICADA          92      1     1 Neumon…
     6    14 J189    NEUMONIA, NO ESPECIFICADA          87      1     2 Neumon…
     7    82 A162    TUBERCULOSIS DE PULMON, SIN ME…    43      1     1 Tuberc…
     8     2 J189    NEUMONIA, NO ESPECIFICADA         101      1     2 Neumon…
     9     6 J189    NEUMONIA, NO ESPECIFICADA          86      1     2 Neumon…
    10    14 J189    NEUMONIA, NO ESPECIFICADA          62      1     1 Neumon…
    # … with 52 more rows

### Métodos para generar listas de errores y warnings.

Las validaciones utilizadas se las divide en 6 indicadores:

Errores (errors) \*:

-   **Asterisco:** Son codigos válidos como códigos adicionales pero no se aceptan como causa básica de muerte(CBM).
-   **No CBD:** Código que no es válido como CBM.
-   **Limitado** a un sexo: restricción del código asociado al género.
-   **Límite de edad:** código limitado a un rango de edad.

Alertas (warnings)\*:

-   **Trivial:** Son condiciones poco probables como causa de muerte.
-   **SMD:** sospecha de muerte materna.

**list\_problems**: Lista los certificados con problemas (errores y/o warnings)

``` r
chequeo$list_problems()
```

    # A tibble: 380 x 13
          id  edad unieda codmuer entity  sexo useless trivial SMD_in age_out
       <int> <int>  <int> <chr>   <chr>  <int> <chr>     <dbl>  <dbl>   <dbl>
     1     6    74      1 I509    INSUF…     2 2             0      0       0
     2     6    88      1 I509    INSUF…     2 2             0      0       0
     3     2    71      1 I509    INSUF…     1 2             0      0       0
     4     6     7      1 G934    ENCEF…     2 2             0      0       0
     5     6    20      1 J189    NEUMO…     2 0             0      1       0
     6     6    85      1 I509    INSUF…     2 2             0      0       0
     7    50    98      1 I10X    HIPER…     2 1             0      0       0
     8     6    80      1 A419    SEPSI…     1 2             0      0       0
     9    66    71      1 J81X    EDEMA…     1 2             0      0       0
    10    82    49      1 R99X    OTRAS…     1 5             0      0       0
    # … with 370 more rows, and 3 more variables: no_cbd <dbl>,
    #   asterisco <dbl>, sex_out <dbl>

### Chequeo de faltantes y desconocidos

**report\_completeness**: Lista los certificados con problemas (errores y/o warnings)

``` r
chequeo$report_completeness()
```

    # A tibble: 5 x 5
      Name    Correct Unknown   NAs pct_correct
      <chr>     <int>   <int> <int>       <dbl>
    1 sexo        997       3     0        99.7
    2 edad        995       5     0        99.5
    3 unieda      996       4     0        99.6
    4 codmuer    1000       0     0       100  
    5 ocloc      1000       0     0       100  

**list\_unknown**: Lista los certificados con problemas (errores y/o warnings)

``` r
chequeo$list_unknown()
```

    # A tibble: 8 x 5
         id  edad unieda codmuer  sexo
      <int> <int>  <int> <chr>   <int>
    1    86     1      1 K631        9
    2     6     0      9 R571        1
    3    18     0      4 P220        2
    4    58    10      4 P000        3
    5     6     0      9 A419        1
    6     6     0      9 I509        2
    7     6    22      1 I678        9
    8    18     0      9 C189        1

### Listar todo.

**list\_all**: Lista todos los certificados.

``` r
chequeo$list_all()
```

    # A tibble: 1,000 x 26
          id  edad unieda codmuer  sexo ocloc code_age_check entity asterisco
       <int> <int>  <int> <chr>   <int> <dbl>          <dbl> <chr>  <lgl>    
     1     6    74      1 I509        2     1         27028. INSUF… FALSE    
     2     6    88      1 I509        2     2         32142  INSUF… FALSE    
     3     2    71      1 I509        1     3         25933. INSUF… FALSE    
     4    54    73      1 J449        1     4         26663. ENFER… FALSE    
     5    50    73      1 C259        2     9         26663. TUMOR… FALSE    
     6     6     7      1 G934        2     1          2557. ENCEF… FALSE    
     7     6    79      1 C329        1     2         28855. TUMOR… FALSE    
     8     6    20      1 J189        2     3          7305  NEUMO… FALSE    
     9    42    50      1 I219        2     4         18262. INFAR… FALSE    
    10     6    72      1 C509        2     9         26298  TUMOR… FALSE    
    # … with 990 more rows, and 17 more variables: trivial <lgl>,
    #   no_cbd <lgl>, useless <chr>, fetal <lgl>,
    #   suspected_maternal_death <int>, sex_limited <chr>,
    #   code_age_upper <dbl>, code_age_lower <dbl>, age_upper <dbl>,
    #   age_lower <dbl>, days_age_upper <dbl>, days_age_lower <dbl>,
    #   SMD_description <chr>, cie10 <chr>, age_out <lgl>, sex_out <lgl>,
    #   SMD_in <lgl>

### Transformar las variables edad y uniedad en Grupos de Edad.

La función `age_codeage()` crea una variable de tipo `factor ordered` con los grupos de edad que utiliza la DEIS al incorporar la edad `age` y la unidad `code_age` (1 = Años, 2 = Meses, 3 = días, 4 = horas, 5 = minutos). Las dos variables ingresadas deben ser de tipo `numeric`.

``` r
library(tidyverse) #librería para análisis de datos
```

    ── Attaching packages ────────────────────────── tidyverse 1.2.1 ──

    ✔ ggplot2 3.1.0       ✔ purrr   0.3.0  
    ✔ tibble  2.0.1       ✔ dplyr   0.8.0.1
    ✔ tidyr   0.8.2       ✔ stringr 1.4.0  
    ✔ readr   1.3.1       ✔ forcats 0.4.0  

    ── Conflicts ───────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()

``` r
test_df2 <- test_df %>% # data.frame
  mutate(grupoEdad = age_codeage(edad, unieda)) # Agrega una nueva variable
```

    Warning in rec_age2day(age, code_age): There are age equals 0 will be
    coerced to 1

``` r
test_df2
```

    # A tibble: 1,000 x 7
          id  edad unieda codmuer  sexo ocloc grupoEdad
       <int> <int>  <int> <chr>   <int> <dbl> <ord>    
     1     6    74      1 I509        2     1 70 - 74  
     2     6    88      1 I509        2     2 85 y +   
     3     2    71      1 I509        1     3 70 - 74  
     4    54    73      1 J449        1     4 70 - 74  
     5    50    73      1 C259        2     9 70 - 74  
     6     6     7      1 G934        2     1 05 - 09  
     7     6    79      1 C329        1     2 75 - 79  
     8     6    20      1 J189        2     3 20 - 24  
     9    42    50      1 I219        2     4 50 - 54  
    10     6    72      1 C509        2     9 70 - 74  
    # … with 990 more rows
