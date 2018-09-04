deistools
================

deistools
=========

deistools es un paquete que simplifica la realización de tareas habituales de las áreas de bioestadística.

Instalación:

``` r
install.packages(c('devtools','tidyverse','readxl','stringr'))
devtools::install_github('diegogarcilazo/deistools')
```

### Funciones de Chequeo

El paquete deistools tiene incorporadas funciones para validar la información de las bases de mortalidad generadas por la DEIS. El primer grupo de validaciones son aquellas utilizadas para el control y reparación de la información.

Las validaciones utilizadas se las divide en 6 indicadores:

Errores (errors) \*:

-   **Asterisco:** Son codigos válidos como códigos adicionales pero no se aceptan como causa básica de muerte(CBM).
-   **No CBD:** Código que no es válido como CBM.
-   **Limitado** a un sexo: restricción del código asociado al género.
-   **Límite de edad:** código limitado a un rango de edad.

Alertas (warnings)\*:

-   **Trivial:** Son condiciones poco probables como causa de muerte.
-   **SMD:** sospecha de muerte materna.

\* Fuente de información: Se utilizó la tablas sugeridas por <http://www.paho.org/hq/index.php?option=com_docman&task=doc_download&gid=23700&Itemid=270&lang=en>

Función cie\_check
------------------

La función cie\_check es la más importante ya que genera el objeto de clase cie\_check que nos permitirá utilizar las demás funciones de chequeo. Al ingresar los datos de edad, unidad de la edad, código de muerte, sexo y las variables que consideremos de identificación de los registros.

``` r
suppressMessages(library(tidyverse))
library(deistools)

obj_check <- test_df %>%
      cie_check(edad, unieda, codmuer, sexo, id)
```

    ## Warning in deistools::rec_age2day(as.numeric(~edad), ~unieda): There are
    ## age equals 0 will be coerced to 1

    ## Warning in rec_age2day(age, code_age): There are age equals 0 will be
    ## coerced to 1

    ## Warning in rec_age2day(age, code_age): There are age equals 0 will be
    ## coerced to 1

    ## Check -> Dataset = . n = 1000
    ## ----------------------------------------------------------------------
    ## Errors and Warnings:
    ## ----------------------------------------------------------------------
    ## 
    ## Errors =  4
    ## Warnings =  376
    ## 
    ## Errors:
    ##            indicator n pct
    ## 1          Age limit 0 0.0
    ## 2      Asterisk code 0 0.0
    ## 3 Limited to one sex 0 0.0
    ## 4             No CBD 4 0.4
    ## 
    ## Warnings:
    ##   indicator   n  pct
    ## 5       SMD  16  1.6
    ## 6   Trivial   1  0.1
    ## 7   Useless 369 36.9
    ## 
    ## Indicators:
    ## 
    ## 1. Age limit: Out of Age limit accepted.
    ## 2. Asterisk: are valid as additional codes but are not accepted as
    ##     a basic cause of death.
    ## 3. Limited to one sex: Identifies restriction codes associated with gender.
    ## 4. No CBD: It is not valid as a Basic Cause of Death.
    ## 5. SMD: Suspected Maternal Death.
    ## 6. Trivial: conditions unlikely to cause death.
    ## 7. Useless Codes.
    ## 
    ## 
    ## ----------------------------------------------------------------------
    ## Useless report:
    ## ----------------------------------------------------------------------
    ##     Useless %All(Death)
    ## 1        16         1.6
    ## 2       220        22.0
    ## 3        33         3.3
    ## 4        37         3.7
    ## 5        63         6.3
    ## All     369        36.9
    ## 
    ## Useless codes explanation:
    ## 1. Causes that cannot or should not be considered as underlying causes of death.
    ## 2. Intermediate causes of death such as heart failure, septicemia, peritonitis,
    ##     osteomyelitis, or pulmonary embolism.
    ## 3. Immediate causes of death that are the final steps in a disease pathway
    ##     leading to death.
    ## 4. Unspecified causes within a larger cause grouping.
    ##    *Author: S. Makela, et al. 2010, Algorithms for enhancing public health utility
    ##              of national causes-of-death data
    ## 5. Ill-defined conditions.
    ## 
    ## 
    ## Useless tables by group age:
    ## 
    ##         Useless Deaths     %
    ## M1            0     19   0.0
    ## M2            1      9  11.1
    ## M3            2      7  28.6
    ## 01            0      2   0.0
    ## 02            1      1 100.0
    ## 03            1      2  50.0
    ## 04            0      1   0.0
    ## 05 - 09       3      7  42.9
    ## 10 - 14       0      1   0.0
    ## 15 - 19       2      4  50.0
    ## 20 - 24       1     13   7.7
    ## 25 - 29       3     11  27.3
    ## 30 - 34       4     13  30.8
    ## 35 - 39       6     20  30.0
    ## 40 - 44       5     17  29.4
    ## 45 - 49      11     28  39.3
    ## 50 - 54      10     33  30.3
    ## 55 - 59      14     52  26.9
    ## 60 - 64      19     72  26.4
    ## 65 - 69      22     75  29.3
    ## 70 - 74      51    118  43.2
    ## 75 - 79      51    122  41.8
    ## 80 - 84      58    141  41.1
    ## 85 y +      101    228  44.3
    ## Sum         366    996  36.7
    ## 
    ## 
    ## ----------------------------------------------------------------------
    ## Notifiable infectous diseases:
    ## ----------------------------------------------------------------------
    ## n =  1000 
    ## ENOs =  62

Con la utilización de funciones adicionales se puede realizar la exploración de este chequeo. La función cie\_summary() nos da un resumen de los errores y alertas que tiene la base.

``` r
obj_check %>% 
 cie_summary()
```

    ## Check -> Dataset = . n = 1000
    ## ----------------------------------------------------------------------
    ## Errors and Warnings:
    ## ----------------------------------------------------------------------
    ## 
    ## Errors =  4
    ## Warnings =  376
    ## 
    ## Errors:
    ##            indicator n pct
    ## 1          Age limit 0 0.0
    ## 2      Asterisk code 0 0.0
    ## 3 Limited to one sex 0 0.0
    ## 4             No CBD 4 0.4
    ## 
    ## Warnings:
    ##   indicator   n  pct
    ## 5       SMD  16  1.6
    ## 6   Trivial   1  0.1
    ## 7   Useless 369 36.9
    ## 
    ## Indicators:
    ## 
    ## 1. Age limit: Out of Age limit accepted.
    ## 2. Asterisk: are valid as additional codes but are not accepted as
    ##     a basic cause of death.
    ## 3. Limited to one sex: Identifies restriction codes associated with gender.
    ## 4. No CBD: It is not valid as a Basic Cause of Death.
    ## 5. SMD: Suspected Maternal Death.
    ## 6. Trivial: conditions unlikely to cause death.
    ## 7. Useless Codes.
    ## 
    ## 
    ## ----------------------------------------------------------------------
    ## Useless report:
    ## ----------------------------------------------------------------------
    ##     Useless %All(Death)
    ## 1        16         1.6
    ## 2       220        22.0
    ## 3        33         3.3
    ## 4        37         3.7
    ## 5        63         6.3
    ## All     369        36.9
    ## 
    ## Useless codes explanation:
    ## 1. Causes that cannot or should not be considered as underlying causes of death.
    ## 2. Intermediate causes of death such as heart failure, septicemia, peritonitis,
    ##     osteomyelitis, or pulmonary embolism.
    ## 3. Immediate causes of death that are the final steps in a disease pathway
    ##     leading to death.
    ## 4. Unspecified causes within a larger cause grouping.
    ##    *Author: S. Makela, et al. 2010, Algorithms for enhancing public health utility
    ##              of national causes-of-death data
    ## 5. Ill-defined conditions.
    ## 
    ## 
    ## Useless tables by group age:
    ## 
    ##         Useless Deaths     %
    ## M1            0     19   0.0
    ## M2            1      9  11.1
    ## M3            2      7  28.6
    ## 01            0      2   0.0
    ## 02            1      1 100.0
    ## 03            1      2  50.0
    ## 04            0      1   0.0
    ## 05 - 09       3      7  42.9
    ## 10 - 14       0      1   0.0
    ## 15 - 19       2      4  50.0
    ## 20 - 24       1     13   7.7
    ## 25 - 29       3     11  27.3
    ## 30 - 34       4     13  30.8
    ## 35 - 39       6     20  30.0
    ## 40 - 44       5     17  29.4
    ## 45 - 49      11     28  39.3
    ## 50 - 54      10     33  30.3
    ## 55 - 59      14     52  26.9
    ## 60 - 64      19     72  26.4
    ## 65 - 69      22     75  29.3
    ## 70 - 74      51    118  43.2
    ## 75 - 79      51    122  41.8
    ## 80 - 84      58    141  41.1
    ## 85 y +      101    228  44.3
    ## Sum         366    996  36.7
    ## 
    ## 
    ## ----------------------------------------------------------------------
    ## Notifiable infectous diseases:
    ## ----------------------------------------------------------------------
    ## n =  1000 
    ## ENOs =  62

Para consultar los registros con errores utilizar la función cie\_tbl\_errors() que construye un data.frame(tibble) con los registros individuales y el error detectado.

``` r
obj_check %>% 
 cie_tbl_errors()
```

    ## # A tibble: 4 x 7
    ##      id codmuer entity                          edad unieda  sexo error   
    ##   <int> <chr>   <chr>                          <int>  <int> <int> <chr>   
    ## 1    78 C793    TUMOR MALIGNO SECUNDARIO DEL …    39      1     2 Not Val…
    ## 2    62 C97X    TUMORES MALIGNOS (PRIMARIOS) …    79      1     2 Not Val…
    ## 3    58 C786    TUMOR MALIGNO SECUNDARIO DEL …    59      1     1 Not Val…
    ## 4     6 C786    TUMOR MALIGNO SECUNDARIO DEL …    87      1     1 Not Val…

Para consultar los registros con alertas utilizar la función cie\_tbl\_warnings() que construye un data.frame(tibble) con los registros individuales y el warning detectado.

``` r
obj_check %>% 
 cie_tbl_warnings()
```

    ## # A tibble: 315 x 8
    ##       id codmuer entity             useless  edad unieda  sexo warning    
    ##    <int> <chr>   <chr>              <chr>   <int>  <int> <int> <chr>      
    ##  1     6 I509    INSUFICIENCIA CAR… 2          74      1     2 Useless co…
    ##  2     6 I509    INSUFICIENCIA CAR… 2          88      1     2 Useless co…
    ##  3     2 I509    INSUFICIENCIA CAR… 2          71      1     1 Useless co…
    ##  4     6 G934    ENCEFALOPATIA NO … 2           7      1     2 Useless co…
    ##  5     6 J189    NEUMONIA, NO ESPE… 0          20      1     2 Suspected …
    ##  6     6 I509    INSUFICIENCIA CAR… 2          85      1     2 Useless co…
    ##  7    50 I10X    HIPERTENSION ESEN… 1          98      1     2 Useless co…
    ##  8     6 A419    SEPSIS, NO ESPECI… 2          80      1     1 Useless co…
    ##  9    66 J81X    EDEMA PULMONAR     2          71      1     1 Useless co…
    ## 10     6 I709    ATEROSCLEROSIS GE… 1          74      1     1 Useless co…
    ## # ... with 305 more rows

Búsqueda de casos de notificación obligatoria
---------------------------------------------

Es útil poder determinar cuales son aquellas enfermedades que se deben notificar para evaluar la reparación con las áreas de epidemiología. Por el momento identifica aquellas causas de origen infeccioso.

Se utiliza la función **code\_enos**. Ejemplo:

``` r
library(tidyverse)
test_df2 <- mutate(test_df, 
       enos = code_enos(x = codmuer, age = edad, age_code = unieda, sex = sexo)
       )

count(test_df2, enos)
```

    ## # A tibble: 8 x 2
    ##   enos                                             n
    ##   <chr>                                        <int>
    ## 1 BRONQUIOLITIS < 2                                1
    ## 2 Diarreas Agudas                                  1
    ## 3 Enfermedad de chagas                             4
    ## 4 Meningitis bacteriana sin especificar agente     1
    ## 5 Neumonía                                        49
    ## 6 No ENOs                                        938
    ## 7 SIDA                                             5
    ## 8 Tuberculosis                                     1

El resultado de esta función es un resumen que se imprime en la consola y la tabla con los datos individuales.
