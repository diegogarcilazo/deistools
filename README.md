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

La función cie\_check genera el objeto de clase cie\_check al ingresar los datos de edad, unidad de la edad, código de muerte, sexo y las variables que consideremos de identificación de los registros.

``` r
suppressMessages(library(tidyverse))
library(deistools)

obj_check <- test_df %>%
      cie_check(edad, unieda, codmuer, sexo, juri)
```

Con la utilización de funciones adicionales se puede realizar la exploración de este chequeo. La función cie\_summary() nos da un resumen de los errores y alertas que tiene la base.

``` r
obj_check %>% 
 cie_summary()
```

    ## 
    ## Check summary
    ## ----------------------------------------------------------------------
    ## Dataset:  . 
    ## n =  1000
    ## Errors =  5
    ## Warnings =  376
    ## 
    ##            indicator   n  pct
    ## 1          Age limit   1  0.1
    ## 2      Asterisk code   0  0.0
    ## 3 Limited to one sex   0  0.0
    ## 4             No CBD   4  0.4
    ## 5                SMD  16  1.6
    ## 6            Trivial   1  0.1
    ## 7            Useless 369 36.9
    ## ----------------------------------------------------------------------
    ## # Asterisk: are valid as additional codes but are not accepted as
    ##      a basic cause of death.
    ## # Trivial: conditions unlikely to cause death.
    ## # No CBD: It is not valid as a Basic Cause of Death.
    ## # Limited to one sex: Identifies restriction codes associated with
    ##       gender.
    ## # Age limit: Out of Age limit accepted.
    ## # SMD: Suspected Maternal Death.

Para consultar los registros con errores utilizar la función cie\_tbl\_errors() que construye un data.frame(tibble) con los registros individuales y el error detectado.

``` r
obj_check %>% 
 cie_tbl_errors()
```

    ## # A tibble: 5 x 7
    ##    juri codmuer entity                         edad unieda  sexo error    
    ##   <int> <chr>   <chr>                         <int>  <int> <int> <chr>    
    ## 1    78 C793    TUMOR MALIGNO SECUNDARIO DEL~    39      1     2 Not Vali~
    ## 2    18 P220    SINDROME DE DIFICULTAD RESPI~     0      4     2 Out of a~
    ## 3    62 C97X    TUMORES MALIGNOS (PRIMARIOS)~    79      1     2 Not Vali~
    ## 4    58 C786    TUMOR MALIGNO SECUNDARIO DEL~    59      1     1 Not Vali~
    ## 5     6 C786    TUMOR MALIGNO SECUNDARIO DEL~    87      1     1 Not Vali~

Para consultar los registros con alertas utilizar la función cie\_tbl\_warnings() que construye un data.frame(tibble) con los registros individuales y el warning detectado.

``` r
obj_check %>% 
 cie_tbl_warnings()
```

    ## # A tibble: 376 x 8
    ##     juri codmuer entity              useless  edad unieda  sexo warning   
    ##    <int> <chr>   <chr>               <chr>   <int>  <int> <int> <chr>     
    ##  1     6 I509    INSUFICIENCIA CARD~ 2          74      1     2 Useless c~
    ##  2     6 I509    INSUFICIENCIA CARD~ 2          88      1     2 Useless c~
    ##  3     2 I509    INSUFICIENCIA CARD~ 2          71      1     1 Useless c~
    ##  4     6 G934    ENCEFALOPATIA NO E~ 2           7      1     2 Useless c~
    ##  5     6 J189    NEUMONIA, NO ESPEC~ 0          20      1     2 Suspected~
    ##  6     6 I509    INSUFICIENCIA CARD~ 2          85      1     2 Useless c~
    ##  7    50 I10X    HIPERTENSION ESENC~ 1          98      1     2 Useless c~
    ##  8     6 A419    SEPSIS, NO ESPECIF~ 2          80      1     1 Useless c~
    ##  9    66 J81X    EDEMA PULMONAR      2          71      1     1 Useless c~
    ## 10    82 R99X    OTRAS CAUSAS MAL D~ 5          49      1     1 Useless c~
    ## # ... with 366 more rows
