---
output: html_document
editor_options: 
  chunk_output_type: inline
---
#deistools
deistools es un paquete que simplifica la realización de tareas habituales de las áreas de bioestadística.

Instalación:
```{r}
install.packages(c('devtools','tidyverse','readxl','stringr'))
devtools::install_github('diegogarcilazo/deistools')
```

###Funciones de Chequeo
Estas funciones permiten tomar una base de mortalidad y realizar algunas validaciones.

La función cie_check genera el objeto de clase cie_check al ingresar los datos de edad, unidad de la edad, 
código de muerte, sexo y las variables que consideremos de identificación de los registros.

```{r}
library(tidyverse)
library(deistools)

obj_check <- test_df %>%
      cie_check(edad, unieda, codmuer, sexo, juri)
```

Con la utilización de funciones adicionales se puede realizar la exploración de este chequeo.
La función cie_summary() nos da un resumen de los errores y alertas que tiene la base.

```{r}

obj_check %>% 
 cie_summary()
 
```


Para consultar los registros con errores utilizar la función cie_tbl_errors() que construye un data.frame(tibble) con los registros individuales y el error detectado.
```{r}

obj_check %>% 
 cie_tbl_errors()
 
```

Para consultar los registros con alertas utilizar la función cie_tbl_warnings() que construye un data.frame(tibble) con los registros individuales y el warning detectado.
```{r}

obj_check %>% 
 cie_tbl_warnings()
 
```
