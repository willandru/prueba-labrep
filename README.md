
<!-- README.md is generated from README.Rmd. Please edit that file. -->
<!-- The code to render this README is stored in .github/workflows/render-readme.yaml -->
<!-- Variables marked with double curly braces will be transformed beforehand: -->
<!-- `packagename` is extracted from the DESCRIPTION file -->
<!-- `gh_repo` is extracted via a special environment variable in GitHub Actions -->

# labrep

<!-- badges: start -->

[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/license/mit/)
[![lifecycle-concept](https://raw.githubusercontent.com/reconverse/reconverse.github.io/master/images/badge-concept.svg)](https://www.reconverse.org/lifecycle.html#concept)
<!-- badges: end -->

labrep provee funciones para la generación automática del informe de
circulación viral.

## Instalación

Puedes decargar la versión de desarrollo de labrep desde
[GitHub](https://github.com/TRACE-LAC/labrep) con el siguiente comando:

``` r
# install.packages("pak")
pak::pak("TRACE-LAC/labrep")
```
## Reporte automatizado

Actualmente, `labrep` provee una plantilla de reporte llamada
`Informe de circulación viral {labrep}`, la cual recibe los siguientes parámetros de
entrada: la base de datos de la Fundación Cardio Infantil, la base de datos de Otros Virus, 
la base de datos de Tosferina, la base de datos historica de los períodos epidemiológicos y 
la semana epidemiológica.

Para hacer uso de la plantilla del reporte puedes seguir los siguientes
pasos:

> 🎥 [¿Cómo generar un reporte con
> labrep?](https://youtu.be/pqzRw5YhP_g)

Para generar el reporte en formato PDF debes instalar LateX. Puedes
instalarlo siguiendo las instrucciones que se encuentran en [R Markdown
Cookbook](https://bookdown.org/yihui/rmarkdown-cookbook/install-latex.html).

Si estas realizando la instalación en computador oficial o de alguna institución, te 
recomendamos seguir los siguientes pasos para evitar inconvenientes con el suso e instalación 
de LateX:

1. Solicitar permisos con la mesa de soporte de la institución para instalar LateX y los siguientes paquetes:
   
* fontenc
* inputenc
* babel-spanish
* floatrow
* fancyhdr
* graphicx
* hyperref
* pdfpages
* xcolor
* colortbl
* caption
* montserrat
* array

## Errores comunes

Si tienes problemas generando el reporte o en la etapa de rendirización del reporte se queda cargando infinitamente, ejecuta los siguientes pasos:
1. En RStudio dirigite al tab de Terminal y ejecuta los siguientes comandos:
  ```
  tlmgr option repository https://mirror.ctan.org/systems/texlive/tlnet
  tlmgr update --self
  tlmgr install fontenc inputenc babel-spanish floatrow fancyhdr graphicx hyperref pdfpages montserrat xcolor colortbl caption array
  ```

## Desarrollo

### Ciclo de vida

Este paquete actualmente está en la fase de *concepto*, como lo define
[RECON software lifecycle](https://www.reconverse.org/lifecycle.html).

### Contribuciones

La contribuciones son bienvenidas vía [pull
requests](https://github.com/TRACE-LAC/labrep/pulls).
