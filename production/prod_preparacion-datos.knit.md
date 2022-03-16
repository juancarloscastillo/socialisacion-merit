---
title: "Preparación de datos - Socialización de la meritocracia"
author: "Castillo, J.C., Iturra, J., Meneses, F. & Venegas, M."
date: '2022-03-16'
output:
  html_document:
    df_print: paged
    theme: paper
    toc: yes
    toc_float: yes
editor_options:
  chunk_output_type: console
---



# Introducción

El presente documento tiene por finalidad la preparación de datos del artículo en proceso "La socialización de la meritocracia: el rol de la familia y la escuela" a cargo de Juan Carlos Castillo. El artículo se desarrolla en el marco del proyecto FONDECYT Regular N°1181239: "Socialización política y educación para la ciudadanía: el rol de la familia y de la escuela." que cuenta con Cristian Cox como investigador principal.  

La estructura para la preparación consistirá en cinco partes. La primera corresponde a cargar las librerías de R para la elaboración del documento. En la segunda se cargarán las bases de datos, información generada desde el proyoecto. En la tercera se seleccionarán las variables a utilizar. En la cuarta, dónde estará el grueso del documento, se procesarán las variables. El procesamiento se divirá en torno a variables relacionadas a apoderados y a estudiantes. Para ambas, se seguirá el siguiente orden: descriptivo, recodificación, etiquetado, descriptivo post-rec y otros ajustes. Además, al lado de cada variable figurará un entreparentesis indicando si corresponde a una variable independiente (Var. Indep.), dependiente (Var. Dep.) o un control (Control) de nivel uno (N1) o dos (N2) Por último, se generará una base de datos procesada para los análisis posteriores, en conjunto a una tabla de descriptivos para la base completa.

# Resumen ejecutivo:
Se listan los puntos mas importantes:

## Registro para revisar

- Para la variable de promedios (obtenido y merecido) existían datos menores a 1 y mayores a 7. En detalle, existían 6 casos para el promedio obtenido y 12 casos para el promedio merecido que no se ajustan a la escala. Provisoriamente fueron recodificados a NA.

- Para la variable de año de nacimiento del apoderado existían datos poco probables. En detalle, existían 10 casos que indican años mayores a 1992. Provisoriamente fueron recodificados a NA.

- ~~Variables de nivel 2 (ingresos por escuela, educación terciaria por escuela y heterogeneidad) cuentan con demasiados NA. Revisar código.~~

- Dos cosas importantes sobre la variable ingresos. La primera es que existen 148 casos correspondientes a NS/NR que han sido recodificados a NA, discutir mantener categoría. La segunda es que en ingresos nivel dos solo existen 7 NA, pertenecientes a los RBD 10452 (solo contestaron estudiantes) y 31047 (ninguno de los seis apoderados de este colegio reportaron sus ingresos). 

## Registro de cambios principales:
- Cambio presentación de las variables. Se dividen entre variables estudiantes y apoderados.
- Se modifica la introducción en función del cambio de estructura.
- Cambio etiquetas variables percepción meritocracia. Se asignan etiquetas más intuitivas para los indicadores.
- Se agrega código de frecuencias posterior a la recodificación por cada variable.
- Se añade tabla descriptiva general, la cual incluye NA's.
- Se arregla construcción de ingreso del hogar. Faltaba una categoría por codificar.
- Se recodifican los valores 8 y 9 en Ns/Nr para las variables de posición política.
- Se desactiva indice flotante ya que choca con la tabla de summarytools.
- Se agregan variables para nueva propuesta
- Se arregla caso mal pareado (897 estudiante). Era un error de tipeo, aunque no cambia el N.
- Se crean variables promedios de percepción meritocrática (en estudiantes y apoderados)
- Se incorpora código detallado sobre nivel educacional (código grupo Tolerancia adaptado)
- Se crean variables factor con su respectivo etiquetado.
- Se centran las variables a utilizar en los modelos (CWC)

# Procesamiento
## Parte 1: Cargar librerías  

Las librerías a utilizar para la elaboración del documento serán:

- *dplyr:* ajuste general de datos
- *sjmisc:* descripción y exploración de base de datos
- *car:* principalmente la función recode para recodificar/agrupar valores de variable
- *stargazer:* para tabla descriptiva  
- *haven:* leer archivos .dta
- *SciViews:* cálculos básicos de matemáticas
- *summarytools:* tablas descriptivas
- *misty* funciones miscelaneas


```r
pacman::p_load(dplyr, sjmisc, car, sjlabelled, stargazer, SciViews, summarytools, misty,sjPlot)
```

## Parte 2: Cargar datos

```r
# Bases de apoderados.
ap <- haven::read_dta("https://github.com/formacionciudadana/data-paces/raw/main/docs/paces/data/base_apoderadosv2.dta")

# Base de estudiantes.
est <- haven::read_dta("https://github.com/formacionciudadana/data-paces/raw/main/docs/paces/data/base_estudiantesv2.dta")

# Arreglar caso 897
ap$FOLIO_EST[ap$FOLIO_EST == 897] <- 892
```

Cabe señalar que dentro de la base de datos existe un caso mal pareado (relación folio estudiante-apoderado). Esto se arregla cambiando el folio erroneo (897 del apoderaod) por su correspondiente.

## Parte 3: Seleccionar variables


```r
# Seleccionar  y renombrar variables de la base de datos de apoderados
ap_proc <- ap %>% dplyr::select(rbd = RBD,
                                folio = FOLIO_EST,
                               region = REGION,
                               dependencia = Dependencia,
                               pos_pol = P23,
                               genero = P39,
                               nacimiento = P40A,
                               personas_hogar = P41,
                               educ = P45,
                               ingresos_tramos = P55,
                               libros_hogar = P47,
                               perc_trabajo_duro = P9D,
                               perc_esfuerzo = P10D,
                               dem_desig = P10A,# Exploración
                               recompensa = P10C) 

# Seleccionar y renombrar variables de la base de datos de estudiantes.                               
est_proc <- est  %>% dplyr::select(rbd = RBD,
                                 region = REGION,
                                 folio = FOLIO,
                                 dependencia = Dependencia,
                                 pos_pol = P59,
                                 genero = P58,
                                 resp_prof = P56B,
                                 educ_padre = P66,
                                 educ_madre = P67,
                                 libros_hogar = P68,
                                 perc_trabajo_duro = P24D,
                                 perc_esfuerzo = P25D,
                                 prom_obt = P27,
                                 prom_mer = P28,
                                 sj_direct = P29,
                                 recompensa = P25C,
                                 tiempo_compartido = P36) 
```

## Parte 4: Procesamiento
### Variables estudiantes

#### Percepción meritocratica estudiantes: Importancia trabajo duro (Var. Dep. N1)
##### Descriptivo

```r
frq(est_proc$perc_trabajo_duro) # Frecuencias
```

```
## 
## P24D Actualmente en Chile, para surgir en la vida ¿Cuán importante esEL TRABAJO  (x) <numeric>
## # total N=1635  valid N=1635  mean=3.52  sd=1.26
## 
## Value |                      Label |   N | Raw % | Valid % | Cum. %
## -------------------------------------------------------------------
##     1 |            Nada importante |  69 |  4.22 |    4.22 |   4.22
##     2 |            Algo importante | 194 | 11.87 |   11.87 |  16.09
##     3 |                 Importante | 413 | 25.26 |   25.26 |  41.35
##     4 |             Muy importante | 912 | 55.78 |   55.78 |  97.13
##     5 | Marca mas de 1 alternativa |   1 |  0.06 |    0.06 |  97.19
##     9 |                         Nr |  46 |  2.81 |    2.81 | 100.00
##  <NA> |                       <NA> |   0 |  0.00 |    <NA> |   <NA>
```

```r
table(est_proc$perc_trabajo_duro)
```

```
## 
##   1   2   3   4   5   9 
##  69 194 413 912   1  46
```

##### Recodificación
Se recodifica a NA las categorías 5 y 9, correspondiente a las etiquetas "Marca más de 1 alternativa" y "Nr".


```r
est_proc$perc_trabajo_duro <- set_na(est_proc$perc_trabajo_duro, na = c(5,9), drop.levels = TRUE, as.tag = FALSE) # Transformar a NA
```


##### Etiquetado


```r
est_proc$perc_trabajo_duro <- set_label(x = est_proc$perc_trabajo_duro,
                                        label = "Percepción meritocracia: Importancia trabajo duro estudiantes") # Etiquetar
```

##### Descriptivo post-rec


```r
frq(est_proc$perc_trabajo_duro) # Frecuencias post recodificación
```

```
## 
## Percepción meritocracia: Importancia trabajo duro estudiantes (x) <numeric>
## # total N=1635  valid N=1588  mean=3.37  sd=0.86
## 
## Value |           Label |   N | Raw % | Valid % | Cum. %
## --------------------------------------------------------
##     1 | Nada importante |  69 |  4.22 |    4.35 |   4.35
##     2 | Algo importante | 194 | 11.87 |   12.22 |  16.56
##     3 |      Importante | 413 | 25.26 |   26.01 |  42.57
##     4 |  Muy importante | 912 | 55.78 |   57.43 | 100.00
##  <NA> |            <NA> |  47 |  2.87 |    <NA> |   <NA>
```

##### Otros ajustes
No aplica.

#### Percepción meritocratica estudiantes: Esfuerzo para salir adelante (Var. Dep. N1)

##### Descriptivo

```r
frq(est_proc$perc_esfuerzo)
```

```
## 
## P25D ¿Cuán de acuerdo o en desacuerdo estás con las siguientes afirmaciones?EN C (x) <numeric>
## # total N=1635  valid N=1635  mean=3.38  sd=1.38
## 
## Value |                      Label |   N | Raw % | Valid % | Cum. %
## -------------------------------------------------------------------
##     1 |          Muy en desacuerdo |  59 |  3.61 |    3.61 |   3.61
##     2 |              En desacuerdo | 227 | 13.88 |   13.88 |  17.49
##     3 |                 De acuerdo | 700 | 42.81 |   42.81 |  60.31
##     4 |             Muy de acuerdo | 582 | 35.60 |   35.60 |  95.90
##     5 | Marca mas de 1 alternativa |   3 |  0.18 |    0.18 |  96.09
##     9 |                         Nr |  64 |  3.91 |    3.91 | 100.00
##  <NA> |                       <NA> |   0 |  0.00 |    <NA> |   <NA>
```

##### Recodificación
Se recodifica a NA las categorías 5 y 9, correspondiente a las etiquetas "Marca más de 1 alternativa" y "Nr".


```r
est_proc$perc_esfuerzo <- set_na(est_proc$perc_esfuerzo, na = c(5,9), drop.levels = TRUE, as.tag = FALSE)
```

##### Etiquetado

```r
est_proc$perc_esfuerzo <- set_label(x = est_proc$perc_esfuerzo, label = "Percepción meritocracia: Esfuerzo para salir adelante (estudiantes)")
```

##### Descriptivo post-rec

```r
frq(est_proc$perc_esfuerzo)
```

```
## 
## Percepción meritocracia: Esfuerzo para salir adelante (estudiantes) (x) <numeric>
## # total N=1635  valid N=1568  mean=3.15  sd=0.80
## 
## Value |             Label |   N | Raw % | Valid % | Cum. %
## ----------------------------------------------------------
##     1 | Muy en desacuerdo |  59 |  3.61 |    3.76 |   3.76
##     2 |     En desacuerdo | 227 | 13.88 |   14.48 |  18.24
##     3 |        De acuerdo | 700 | 42.81 |   44.64 |  62.88
##     4 |    Muy de acuerdo | 582 | 35.60 |   37.12 | 100.00
##  <NA> |              <NA> |  67 |  4.10 |    <NA> |   <NA>
```

##### Otros ajustes
Creación variable: promedio de percepción de meritocracia en estudiantes

```r
# Crear variable
est_proc$prom_percmer_est <- ((est_proc$perc_trabajo_duro + est_proc$perc_esfuerzo)/2)

# Etiquetar
est_proc$prom_percmer_est <- set_label(x = est_proc$prom_percmer_est, label = "Promedio Percepción meritocratica estudiantes")
```

Creación variables factores 

```r
# Esfuerzo
# est_proc$perc_esfuerzo_factor <- factor(est_proc$perc_esfuerzo, levels = c(1,2,3,4), labels = c("Muy en desacuerdo", "En desacuerdo", "De acuerdo", "Muy de acuerdo"))

est_proc$perc_esfuerzo_factor <- factor(est_proc$perc_esfuerzo, levels = c(1,2,3,4), labels = c("Strongly Disagree", 
"Disagree", 
"Agree", 
"Strongly Agree"))


# Etiquetar
# est_proc$perc_esfuerzo_factor <- set_label(x = est_proc$perc_esfuerzo_factor, label = "Percepción meritocracia: Esfuerzo para salir adelante (estudiantes)")

est_proc$perc_esfuerzo_factor <- set_label(x = est_proc$perc_esfuerzo_factor, label = "Perception of meritocracy: Effort to get ahead (students)")

# Trabajo duro
# est_proc$perc_trabajo_duro_factor <- factor(est_proc$perc_trabajo_duro, levels = c(1,2,3,4), labels = c("Nada importante", "Algo importante", "Importante", "Muy importante"))

est_proc$perc_trabajo_duro_factor <- factor(est_proc$perc_trabajo_duro, levels = c(1,2,3,4), labels = c("Not important", "Somewhat important", "Important", "Very important"))

# Etiquetar
# est_proc$perc_trabajo_duro_factor <- set_label(x = est_proc$perc_trabajo_duro_factor, label = "Percepción meritocracia: Importancia trabajo duro (estudiantes)")

est_proc$perc_trabajo_duro_factor <- set_label(x = est_proc$perc_trabajo_duro_factor, label = "Perception of meritocracy: Importance of hard work (students)")

# Finales
freq(est_proc$perc_esfuerzo_factor)
```

```
##                            Freq    Perc  V.Perc
##  Value   Strongly Disagree   59   3.61%   3.76%
##          Disagree           227  13.88%  14.48%
##          Agree              700  42.81%  44.64%
##          Strongly Agree     582  35.60%  37.12%
##          Total             1568  95.90% 100.00%
##  Missing NA                  67   4.10%        
##  Total                     1635 100.00%
```

```r
freq(est_proc$perc_trabajo_duro_factor)
```

```
##                             Freq    Perc  V.Perc
##  Value   Not important        69   4.22%   4.35%
##          Somewhat important  194  11.87%  12.22%
##          Important           413  25.26%  26.01%
##          Very important      912  55.78%  57.43%
##          Total              1588  97.13% 100.00%
##  Missing NA                   47   2.87%        
##  Total                      1635 100.00%
```


#### Sentido de justicia (indirecto): Promedio obtenido y merecido (Var. Indep. N1)
##### Descriptivo

```r
# Promedio obtenido
summary(est_proc$prom_obt) # Descriptivo
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    1.00    5.40    5.90   13.18    6.30   99.00
```

```r
frq(est_proc$prom_obt)
```

```
## 
## P27 ¿Qué promedio de nota obtuviste el año pasado? Si no lo recuerdas exactament (x) <numeric>
## # total N=1635  valid N=1635  mean=13.18  sd=25.24
## 
## Value | Label |   N | Raw % | Valid % | Cum. %
## ----------------------------------------------
##  1.00 |     1 |   1 |  0.06 |    0.06 |   0.06
##  1.10 |   1.1 |   1 |  0.06 |    0.06 |   0.12
##  2.00 |     2 |   1 |  0.06 |    0.06 |   0.18
##  2.90 |   2.9 |   1 |  0.06 |    0.06 |   0.24
##  3.00 |     3 |   1 |  0.06 |    0.06 |   0.31
##  3.50 |   3.5 |   1 |  0.06 |    0.06 |   0.37
##  3.60 |   3.6 |   2 |  0.12 |    0.12 |   0.49
##  3.80 |   3.8 |   1 |  0.06 |    0.06 |   0.55
##  3.90 |   3.9 |   1 |  0.06 |    0.06 |   0.61
##  4.00 |     4 |   3 |  0.18 |    0.18 |   0.80
##  4.10 |   4.1 |   3 |  0.18 |    0.18 |   0.98
##  4.20 |   4.2 |   2 |  0.12 |    0.12 |   1.10
##  4.30 |   4.3 |   7 |  0.43 |    0.43 |   1.53
##  4.40 |   4.4 |   4 |  0.24 |    0.24 |   1.77
##  4.50 |   4.5 |  18 |  1.10 |    1.10 |   2.87
##  4.60 |   4.6 |  12 |  0.73 |    0.73 |   3.61
##  4.70 |   4.7 |  15 |  0.92 |    0.92 |   4.53
##  4.80 |   4.8 |  23 |  1.41 |    1.41 |   5.93
##  4.90 |   4.9 |  26 |  1.59 |    1.59 |   7.52
##  5.00 |     5 |  87 |  5.32 |    5.32 |  12.84
##  5.10 |   5.1 |  41 |  2.51 |    2.51 |  15.35
##  5.20 |   5.2 |  56 |  3.43 |    3.43 |  18.78
##  5.30 |   5.3 |  60 |  3.67 |    3.67 |  22.45
##  5.40 |   5.4 |  74 |  4.53 |    4.53 |  26.97
##  5.50 |   5.5 |  96 |  5.87 |    5.87 |  32.84
##  5.60 |   5.6 |  74 |  4.53 |    4.53 |  37.37
##  5.70 |   5.7 |  87 |  5.32 |    5.32 |  42.69
##  5.80 |   5.8 | 105 |  6.42 |    6.42 |  49.11
##  5.90 |   5.9 |  99 |  6.06 |    6.06 |  55.17
##  6.00 |     6 | 105 |  6.42 |    6.42 |  61.59
##  6.10 |   6.1 |  92 |  5.63 |    5.63 |  67.22
##  6.20 |   6.2 |  76 |  4.65 |    4.65 |  71.87
##  6.30 |   6.3 |  84 |  5.14 |    5.14 |  77.00
##  6.40 |   6.4 |  56 |  3.43 |    3.43 |  80.43
##  6.50 |   6.5 |  58 |  3.55 |    3.55 |  83.98
##  6.60 |   6.6 |  40 |  2.45 |    2.45 |  86.42
##  6.70 |   6.7 |  37 |  2.26 |    2.26 |  88.69
##  6.80 |   6.8 |  32 |  1.96 |    1.96 |  90.64
##  6.90 |   6.9 |  12 |  0.73 |    0.73 |  91.38
##  7.00 |     7 |   5 |  0.31 |    0.31 |  91.68
##  7.10 |   7.1 |   1 |  0.06 |    0.06 |  91.74
##  8.50 |   8.5 |   1 |  0.06 |    0.06 |  91.80
## 10.00 |    10 |   2 |  0.12 |    0.12 |  91.93
## 15.00 |    15 |   2 |  0.12 |    0.12 |  92.05
## 99.00 |    Nr | 130 |  7.95 |    7.95 | 100.00
##  <NA> |  <NA> |   0 |  0.00 |    <NA> |   <NA>
```

```r
# Promedio merecido
summary(est_proc$prom_mer)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00    5.70    6.00   14.75    6.60   99.00
```

```r
frq(est_proc$prom_mer)
```

```
## 
## P28 Y, ¿Qué promedio de nota piensas que merecías? (x) <numeric>
## # total N=1635  valid N=1635  mean=14.75  sd=27.17
## 
## Value | Label |   N | Raw % | Valid % | Cum. %
## ----------------------------------------------
##  0.00 |     0 |   1 |  0.06 |    0.06 |   0.06
##  1.00 |     1 |   1 |  0.06 |    0.06 |   0.12
##  1.60 |   1.6 |   1 |  0.06 |    0.06 |   0.18
##  2.00 |     2 |   6 |  0.37 |    0.37 |   0.55
##  3.00 |     3 |   5 |  0.31 |    0.31 |   0.86
##  4.00 |     4 |  13 |  0.80 |    0.80 |   1.65
##  4.20 |   4.2 |   2 |  0.12 |    0.12 |   1.77
##  4.30 |   4.3 |   2 |  0.12 |    0.12 |   1.90
##  4.40 |   4.4 |   2 |  0.12 |    0.12 |   2.02
##  4.50 |   4.5 |  16 |  0.98 |    0.98 |   3.00
##  4.60 |   4.6 |   4 |  0.24 |    0.24 |   3.24
##  4.70 |   4.7 |   5 |  0.31 |    0.31 |   3.55
##  4.80 |   4.8 |   5 |  0.31 |    0.31 |   3.85
##  4.90 |   4.9 |  11 |  0.67 |    0.67 |   4.53
##  5.00 |     5 |  83 |  5.08 |    5.08 |   9.60
##  5.10 |   5.1 |  10 |  0.61 |    0.61 |  10.21
##  5.20 |   5.2 |  17 |  1.04 |    1.04 |  11.25
##  5.30 |   5.3 |  21 |  1.28 |    1.28 |  12.54
##  5.40 |   5.4 |  29 |  1.77 |    1.77 |  14.31
##  5.50 |   5.5 | 102 |  6.24 |    6.24 |  20.55
##  5.60 |   5.6 |  40 |  2.45 |    2.45 |  23.00
##  5.70 |   5.7 |  41 |  2.51 |    2.51 |  25.50
##  5.80 |   5.8 |  72 |  4.40 |    4.40 |  29.91
##  5.90 |   5.9 |  52 |  3.18 |    3.18 |  33.09
##  6.00 |     6 | 302 | 18.47 |   18.47 |  51.56
##  6.10 |   6.1 |  45 |  2.75 |    2.75 |  54.31
##  6.20 |   6.2 |  69 |  4.22 |    4.22 |  58.53
##  6.30 |   6.3 |  73 |  4.46 |    4.46 |  63.00
##  6.40 |   6.4 |  52 |  3.18 |    3.18 |  66.18
##  6.50 |   6.5 | 137 |  8.38 |    8.38 |  74.56
##  6.60 |   6.6 |  45 |  2.75 |    2.75 |  77.31
##  6.70 |   6.7 |  41 |  2.51 |    2.51 |  79.82
##  6.80 |   6.8 |  55 |  3.36 |    3.36 |  83.18
##  6.90 |   6.9 |  20 |  1.22 |    1.22 |  84.40
##  7.00 |     7 |  91 |  5.57 |    5.57 |  89.97
##  7.20 |   7.2 |   1 |  0.06 |    0.06 |  90.03
##  7.80 |   7.8 |   2 |  0.12 |    0.12 |  90.15
##  8.00 |     8 |   3 |  0.18 |    0.18 |  90.34
##  9.00 |     9 |   1 |  0.06 |    0.06 |  90.40
## 10.00 |    10 |   1 |  0.06 |    0.06 |  90.46
## 15.00 |    15 |   2 |  0.12 |    0.12 |  90.58
## 90.00 |    90 |   1 |  0.06 |    0.06 |  90.64
## 99.00 |    Nr | 153 |  9.36 |    9.36 | 100.00
##  <NA> |  <NA> |   0 |  0.00 |    <NA> |   <NA>
```

##### Recodificación
Se recodifica a NA las siguientes situaciones:
- Datos 99, correspondiente a No responde.
- Datos menores a 1s
- Datos mayores a 7


```r
# Promedio obtenido
est_proc$prom_obt[est_proc$prom_obt == 99] <- NA
est_proc$prom_obt[est_proc$prom_obt < 1 | est_proc$prom_obt > 7] <- NA

# Promedio merecido
est_proc$prom_mer[est_proc$prom_mer == 99] <- NA
est_proc$prom_mer[est_proc$prom_mer < 1 | est_proc$prom_mer > 7] <- NA
```

##### Etiquetado

```r
# Promedio obtenido
est_proc$prom_obt <- set_label(x = est_proc$prom_obt, label = "Grade point average achieved") 

# Promedio merecido
est_proc$prom_mer <- set_label(x = est_proc$prom_mer, label = "Grade Point Average Deserved") 
```

##### Descriptivio post-rec

```r
# Promedio obtenido
summary(est_proc$prom_obt) # Descriptivo
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   1.000   5.400   5.800   5.745   6.200   7.000     136
```

```r
frq(est_proc$prom_obt)
```

```
## 
## Grade point average achieved (x) <numeric>
## # total N=1635  valid N=1499  mean=5.75  sd=0.63
## 
## Value | Label |   N | Raw % | Valid % | Cum. %
## ----------------------------------------------
##  1.00 |     1 |   1 |  0.06 |    0.07 |   0.07
##  1.10 |   1.1 |   1 |  0.06 |    0.07 |   0.13
##  2.00 |     2 |   1 |  0.06 |    0.07 |   0.20
##  2.90 |   2.9 |   1 |  0.06 |    0.07 |   0.27
##  3.00 |     3 |   1 |  0.06 |    0.07 |   0.33
##  3.50 |   3.5 |   1 |  0.06 |    0.07 |   0.40
##  3.60 |   3.6 |   2 |  0.12 |    0.13 |   0.53
##  3.80 |   3.8 |   1 |  0.06 |    0.07 |   0.60
##  3.90 |   3.9 |   1 |  0.06 |    0.07 |   0.67
##  4.00 |     4 |   3 |  0.18 |    0.20 |   0.87
##  4.10 |   4.1 |   3 |  0.18 |    0.20 |   1.07
##  4.20 |   4.2 |   2 |  0.12 |    0.13 |   1.20
##  4.30 |   4.3 |   7 |  0.43 |    0.47 |   1.67
##  4.40 |   4.4 |   4 |  0.24 |    0.27 |   1.93
##  4.50 |   4.5 |  18 |  1.10 |    1.20 |   3.14
##  4.60 |   4.6 |  12 |  0.73 |    0.80 |   3.94
##  4.70 |   4.7 |  15 |  0.92 |    1.00 |   4.94
##  4.80 |   4.8 |  23 |  1.41 |    1.53 |   6.47
##  4.90 |   4.9 |  26 |  1.59 |    1.73 |   8.21
##  5.00 |     5 |  87 |  5.32 |    5.80 |  14.01
##  5.10 |   5.1 |  41 |  2.51 |    2.74 |  16.74
##  5.20 |   5.2 |  56 |  3.43 |    3.74 |  20.48
##  5.30 |   5.3 |  60 |  3.67 |    4.00 |  24.48
##  5.40 |   5.4 |  74 |  4.53 |    4.94 |  29.42
##  5.50 |   5.5 |  96 |  5.87 |    6.40 |  35.82
##  5.60 |   5.6 |  74 |  4.53 |    4.94 |  40.76
##  5.70 |   5.7 |  87 |  5.32 |    5.80 |  46.56
##  5.80 |   5.8 | 105 |  6.42 |    7.00 |  53.57
##  5.90 |   5.9 |  99 |  6.06 |    6.60 |  60.17
##  6.00 |     6 | 105 |  6.42 |    7.00 |  67.18
##  6.10 |   6.1 |  92 |  5.63 |    6.14 |  73.32
##  6.20 |   6.2 |  76 |  4.65 |    5.07 |  78.39
##  6.30 |   6.3 |  84 |  5.14 |    5.60 |  83.99
##  6.40 |   6.4 |  56 |  3.43 |    3.74 |  87.73
##  6.50 |   6.5 |  58 |  3.55 |    3.87 |  91.59
##  6.60 |   6.6 |  40 |  2.45 |    2.67 |  94.26
##  6.70 |   6.7 |  37 |  2.26 |    2.47 |  96.73
##  6.80 |   6.8 |  32 |  1.96 |    2.13 |  98.87
##  6.90 |   6.9 |  12 |  0.73 |    0.80 |  99.67
##  7.00 |     7 |   5 |  0.31 |    0.33 | 100.00
## 99.00 |    Nr |   0 |  0.00 |    0.00 | 100.00
##  <NA> |  <NA> | 136 |  8.32 |    <NA> |   <NA>
```

```r
# Promedio merecido
summary(est_proc$prom_mer)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   1.000   5.600   6.000   5.975   6.500   7.000     165
```

```r
frq(est_proc$prom_mer)
```

```
## 
## Grade Point Average Deserved (x) <numeric>
## # total N=1635  valid N=1470  mean=5.97  sd=0.70
## 
## Value | Label |   N | Raw % | Valid % | Cum. %
## ----------------------------------------------
##  1.00 |     1 |   1 |  0.06 |    0.07 |   0.07
##  1.60 |   1.6 |   1 |  0.06 |    0.07 |   0.14
##  2.00 |     2 |   6 |  0.37 |    0.41 |   0.54
##  3.00 |     3 |   5 |  0.31 |    0.34 |   0.88
##  4.00 |     4 |  13 |  0.80 |    0.88 |   1.77
##  4.20 |   4.2 |   2 |  0.12 |    0.14 |   1.90
##  4.30 |   4.3 |   2 |  0.12 |    0.14 |   2.04
##  4.40 |   4.4 |   2 |  0.12 |    0.14 |   2.18
##  4.50 |   4.5 |  16 |  0.98 |    1.09 |   3.27
##  4.60 |   4.6 |   4 |  0.24 |    0.27 |   3.54
##  4.70 |   4.7 |   5 |  0.31 |    0.34 |   3.88
##  4.80 |   4.8 |   5 |  0.31 |    0.34 |   4.22
##  4.90 |   4.9 |  11 |  0.67 |    0.75 |   4.97
##  5.00 |     5 |  83 |  5.08 |    5.65 |  10.61
##  5.10 |   5.1 |  10 |  0.61 |    0.68 |  11.29
##  5.20 |   5.2 |  17 |  1.04 |    1.16 |  12.45
##  5.30 |   5.3 |  21 |  1.28 |    1.43 |  13.88
##  5.40 |   5.4 |  29 |  1.77 |    1.97 |  15.85
##  5.50 |   5.5 | 102 |  6.24 |    6.94 |  22.79
##  5.60 |   5.6 |  40 |  2.45 |    2.72 |  25.51
##  5.70 |   5.7 |  41 |  2.51 |    2.79 |  28.30
##  5.80 |   5.8 |  72 |  4.40 |    4.90 |  33.20
##  5.90 |   5.9 |  52 |  3.18 |    3.54 |  36.73
##  6.00 |     6 | 302 | 18.47 |   20.54 |  57.28
##  6.10 |   6.1 |  45 |  2.75 |    3.06 |  60.34
##  6.20 |   6.2 |  69 |  4.22 |    4.69 |  65.03
##  6.30 |   6.3 |  73 |  4.46 |    4.97 |  70.00
##  6.40 |   6.4 |  52 |  3.18 |    3.54 |  73.54
##  6.50 |   6.5 | 137 |  8.38 |    9.32 |  82.86
##  6.60 |   6.6 |  45 |  2.75 |    3.06 |  85.92
##  6.70 |   6.7 |  41 |  2.51 |    2.79 |  88.71
##  6.80 |   6.8 |  55 |  3.36 |    3.74 |  92.45
##  6.90 |   6.9 |  20 |  1.22 |    1.36 |  93.81
##  7.00 |     7 |  91 |  5.57 |    6.19 | 100.00
## 99.00 |    Nr |   0 |  0.00 |    0.00 | 100.00
##  <NA> |  <NA> | 165 | 10.09 |    <NA> |   <NA>
```

##### Otros ajustes
Se elaboran dos variables nuevas a partir del cálculo para evaluación de justicia de Jasso (1980). La primera corresponde al logaritmo natural de la proporción entre el promedio obtenido y el promedio merecido. La segunda corresponde a la misma proporción sin el logaritmo.


```r
# Cálculo sentido justicia original
est_proc$sj_indirect_ln <-  ln(est_proc$prom_obt/est_proc$prom_mer)

# Cálculo sentido justicia sin ln
est_proc$sj_indirect_noln <-  est_proc$prom_obt/est_proc$prom_mer

##### ---- Descriptivos variables nuevas ----
# Sentido de justicia
summary(est_proc$sj_indirect_ln)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
## -0.88120 -0.06899 -0.03226 -0.03773  0.00000  1.06471      171
```

```r
# Sentido de justicia sin ln
summary(est_proc$sj_indirect_noln)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.4143  0.9333  0.9683  0.9692  1.0000  2.9000     171
```

```r
##### ---- Etiquetado varibles nuevas ----
# Sentido de justicia
est_proc$sj_indirect_ln <- set_label(x = est_proc$sj_indirect_ln, label = "Sense of justice in grades - Indirect (ln)")

# Sentido de justicia sin ln
est_proc$sj_indirect_noln <- set_label(x = est_proc$sj_indirect_noln, label = "Sentido de Justicia indirecto")
```

* recodificamos sentido de justicia indirecto en grupos:
  * < 0 son "Nota subrecompensada"
  * = 1 son "Nota justa"
  * > 1 son "Nota sobrerecompensada"
  

```r
est_proc$sj_indirect_cat<- car::recode(var = est_proc$sj_indirect_noln,
                                       "lo:0.99='Nota subrecompensada';1='Nota justa';1.01:hi='Nota sobrerecompensada'",
                                       as.factor = T) 
est_proc$sj_indirect_cat<- set_label(x = est_proc$sj_indirect_cat, label = "Sentido de Justicia indirecto (categórico)")
frq(est_proc$sj_indirect_cat)
```

```
## 
## Sentido de Justicia indirecto (categórico) (x) <categorical>
## # total N=1635  valid N=1464  mean=2.43  sd=0.85
## 
## Value                  |   N | Raw % | Valid % | Cum. %
## -------------------------------------------------------
## Nota justa             | 348 | 21.28 |   23.77 |  23.77
## Nota sobrerecompensada | 139 |  8.50 |    9.49 |  33.27
## Nota subrecompensada   | 977 | 59.76 |   66.73 | 100.00
## <NA>                   | 171 | 10.46 |    <NA> |   <NA>
```

* Sentido de justicia indirecto en cuartiles


```r
est_proc$sj_indirect_qtil <- ntile(est_proc$sj_indirect_noln,n = 4)
est_proc$sj_indirect_qtil <- factor(est_proc$sj_indirect_qtil,
                                    levels = c(1:4),
                                    labels = c("Cuartil 1","Cuartil 2","Cuartil 3","Cuartil 4"))
est_proc$sj_indirect_qtil<- set_label(x = est_proc$sj_indirect_qtil, label = "Sentido de Justicia indirecto (cuartiles)")
frq(est_proc$sj_indirect_qtil)
```

```
## 
## Sentido de Justicia indirecto (cuartiles) (x) <categorical>
## # total N=1635  valid N=1464  mean=2.50  sd=1.12
## 
## Value     |   N | Raw % | Valid % | Cum. %
## ------------------------------------------
## Cuartil 1 | 366 | 22.39 |      25 |     25
## Cuartil 2 | 366 | 22.39 |      25 |     50
## Cuartil 3 | 366 | 22.39 |      25 |     75
## Cuartil 4 | 366 | 22.39 |      25 |    100
## <NA>      | 171 | 10.46 |    <NA> |   <NA>
```

#### Sentido de justicia (directo) (Var. Indep. N1)
##### Descriptivo

```r
frq(est_proc$sj_direct)
```

```
## 
## P29 Tomando en cuenta el tiempo que le dedico a mis estudios, las notas que me s (x) <numeric>
## # total N=1635  valid N=1635  mean=2.28  sd=1.64
## 
## Value |                      Label |   N | Raw % | Valid % | Cum. %
## -------------------------------------------------------------------
##     1 |   Menos de las que merezco | 349 | 21.35 |   21.35 |  21.35
##     2 |            Las que merezco | 978 | 59.82 |   59.82 |  81.16
##     3 |     Más de las que merezco | 214 | 13.09 |   13.09 |  94.25
##     4 | Marca mas de 1 alternativa |  14 |  0.86 |    0.86 |  95.11
##     9 |                         Nr |  80 |  4.89 |    4.89 | 100.00
##  <NA> |                       <NA> |   0 |  0.00 |    <NA> |   <NA>
```

##### Recodificación
Se transforma a NA los valores 4 y 9, correspondientes a las categorías "Marca mas de una alternativa" y "Nr"


```r
est_proc$sj_direct <- set_na(est_proc$sj_direct, na = c(4,9), drop.levels = TRUE, as.tag = FALSE)
```

##### Etiquetado

```r
est_proc$sj_direct <- set_label(x = est_proc$sj_direct, label = "Sense of justice in grades -  Direct")
```

##### Descriptivo post-rec

```r
frq(est_proc$sj_direct)
```

```
## 
## Sense of justice in grades -  Direct (x) <numeric>
## # total N=1635  valid N=1541  mean=1.91  sd=0.60
## 
## Value |                    Label |   N | Raw % | Valid % | Cum. %
## -----------------------------------------------------------------
##     1 | Menos de las que merezco | 349 | 21.35 |   22.65 |  22.65
##     2 |          Las que merezco | 978 | 59.82 |   63.47 |  86.11
##     3 |   Más de las que merezco | 214 | 13.09 |   13.89 | 100.00
##  <NA> |                     <NA> |  94 |  5.75 |    <NA> |   <NA>
```

##### Otros ajustes
Crear variable factor


```r
# Factor
# est_proc$sj_direct_factor <- factor(est_proc$sj_direct, levels = c(1,2,3), labels = c("Menos de las que merezco", "Las que merezco", "Más de las que merezco"))

est_proc$sj_direct_factor <- factor(est_proc$sj_direct, levels = c(1,2,3), labels = c("Less than I deserve", "The ones I deserve", "More than I deserve"))


est_proc$sj_direct_factor <- relevel(est_proc$sj_direct_factor,ref = 2)

frq(est_proc$sj_direct_factor)
```

```
## 
## x <categorical>
## # total N=1635  valid N=1541  mean=1.50  sd=0.73
## 
## Value               |   N | Raw % | Valid % | Cum. %
## ----------------------------------------------------
## The ones I deserve  | 978 | 59.82 |   63.47 |  63.47
## Less than I deserve | 349 | 21.35 |   22.65 |  86.11
## More than I deserve | 214 | 13.09 |   13.89 | 100.00
## <NA>                |  94 |  5.75 |    <NA> |   <NA>
```

```r
# Etiquetar
est_proc$sj_direct_factor <- set_label(x = est_proc$sj_direct_factor, label = "Sense of justice in grades -  Direct")
```

#### NSE: Educacion (Var. Indep. N1)
##### Descriptivo

```r
# Cuestionario estudiantes
frq(est_proc$educ_madre)
```

```
## 
## P67 ¿Cuál es el último curso o nivel de estudios que alcanzó tu madre? (x) <numeric>
## # total N=1635  valid N=1635  mean=4.08  sd=2.00
## 
## Value |                                                                        Label |   N | Raw % | Valid % | Cum. %
## ---------------------------------------------------------------------------------------------------------------------
##     1 |                                                       No completó 8vo Básico |  89 |  5.44 |    5.44 |   5.44
##     2 |                                                                   8vo básico | 118 |  7.22 |    7.22 |  12.66
##     3 |                                                              Educación media | 566 | 34.62 |   34.62 |  47.28
##     4 | Educación Técnica superior (Instituto profesional o Centro de Formación Técn | 367 | 22.45 |   22.45 |  69.72
##     5 |  Una carrera en la Universidad o estudios de posgrado (Magíster o Doctorado) | 314 | 19.20 |   19.20 |  88.93
##     8 |                                                   Marca mas de 1 alternativa |  14 |  0.86 |    0.86 |  89.79
##     9 |                                                                           Nr | 167 | 10.21 |   10.21 | 100.00
##  <NA> |                                                                         <NA> |   0 |  0.00 |    <NA> |   <NA>
```

```r
frq(est_proc$educ_padre)
```

```
## 
## P66 ¿Cuál es el último curso o nivel de estudios que alcanzó tu padre? (x) <numeric>
## # total N=1635  valid N=1635  mean=4.23  sd=2.14
## 
## Value |                                                                        Label |   N | Raw % | Valid % | Cum. %
## ---------------------------------------------------------------------------------------------------------------------
##     1 |                                                       No completó 8vo Básico |  87 |  5.32 |    5.32 |   5.32
##     2 |                                                                   8vo básico | 123 |  7.52 |    7.52 |  12.84
##     3 |                                                              Educación media | 541 | 33.09 |   33.09 |  45.93
##     4 | Educación Técnica superior (Instituto profesional o Centro de Formación Técn | 338 | 20.67 |   20.67 |  66.61
##     5 |  Una carrera en la Universidad o estudios de posgrado (Magíster o Doctorado) | 322 | 19.69 |   19.69 |  86.30
##     8 |                                                   Marca mas de 1 alternativa |  16 |  0.98 |    0.98 |  87.28
##     9 |                                                                           Nr | 208 | 12.72 |   12.72 | 100.00
##  <NA> |                                                                         <NA> |   0 |  0.00 |    <NA> |   <NA>
```
##### Recodificacion
Se transforma el valor 9 (Nr) y el 8 (Marca mas de 1 alternativa) en NA em ambas variables.


```r
# Cuestionario estudiantes
est_proc$educ_madre <- set_na(est_proc$educ_madre, na = c(8,9), drop.levels = TRUE, as.tag = FALSE)
est_proc$educ_padre <- set_na(est_proc$educ_padre, na = c(8,9), drop.levels = TRUE, as.tag = FALSE)
```

##### Etiquetado

```r
# Cuestionario estudiantes
est_proc$educ_madre <- set_label(x = est_proc$educ_madre, label = "Nivel educacional más alto de la madre")
est_proc$educ_padre <- set_label(x = est_proc$educ_padre, label = "Nivel educacional más alto del padre")
```

##### Descriptivo post-rec

```r
# Cuestionario estudiantes
frq(est_proc$educ_madre)
```

```
## 
## Nivel educacional más alto de la madre (x) <numeric>
## # total N=1635  valid N=1454  mean=3.48  sd=1.10
## 
## Value |                                                                        Label |   N | Raw % | Valid % | Cum. %
## ---------------------------------------------------------------------------------------------------------------------
##     1 |                                                       No completó 8vo Básico |  89 |  5.44 |    6.12 |   6.12
##     2 |                                                                   8vo básico | 118 |  7.22 |    8.12 |  14.24
##     3 |                                                              Educación media | 566 | 34.62 |   38.93 |  53.16
##     4 | Educación Técnica superior (Instituto profesional o Centro de Formación Técn | 367 | 22.45 |   25.24 |  78.40
##     5 |  Una carrera en la Universidad o estudios de posgrado (Magíster o Doctorado) | 314 | 19.20 |   21.60 | 100.00
##  <NA> |                                                                         <NA> | 181 | 11.07 |    <NA> |   <NA>
```

```r
frq(est_proc$educ_padre)
```

```
## 
## Nivel educacional más alto del padre (x) <numeric>
## # total N=1635  valid N=1411  mean=3.49  sd=1.12
## 
## Value |                                                                        Label |   N | Raw % | Valid % | Cum. %
## ---------------------------------------------------------------------------------------------------------------------
##     1 |                                                       No completó 8vo Básico |  87 |  5.32 |    6.17 |   6.17
##     2 |                                                                   8vo básico | 123 |  7.52 |    8.72 |  14.88
##     3 |                                                              Educación media | 541 | 33.09 |   38.34 |  53.22
##     4 | Educación Técnica superior (Instituto profesional o Centro de Formación Técn | 338 | 20.67 |   23.95 |  77.18
##     5 |  Una carrera en la Universidad o estudios de posgrado (Magíster o Doctorado) | 322 | 19.69 |   22.82 | 100.00
##  <NA> |                                                                         <NA> | 224 | 13.70 |    <NA> |   <NA>
```

##### Otros ajustes
Creación variable "Nivel educacional más alto de los padres"

```r
# Crear variable nueva
est_proc$educ_padres <- ifelse(est_proc$educ_madre>est_proc$educ_padre,est_proc$educ_madre,est_proc$educ_padre)

# Etiquetar variable nueva
est_proc$educ_padres <- set_label(x = est_proc$educ_padres,label = "Nivel educacional más alto de los padres")
 
# Factor
est_proc$educ_padres_factor <- factor(est_proc$educ_padres, levels = c(1,2,3,4,5), labels = c("No completó 8vo Básico", "8vo básico", "Educación media", "Educación Técnica superior", "Educación universitaria o posgrados"))

# Etiquetar factor
est_proc$educ_padres_factor <- set_label(x = est_proc$educ_padres, label = "Nivel educacional más alto de los padres")
```

#### Género (Control N1)
##### Descriptivo

```r
# Genero estudiantes
frq(est_proc$genero)
```

```
## 
## P58 ¿Cuál es tu género? (x) <numeric>
## # total N=1635  valid N=1635  mean=1.61  sd=1.02
## 
## Value |                      Label |   N | Raw % | Valid % | Cum. %
## -------------------------------------------------------------------
##     1 |                     Hombre | 834 | 51.01 |   51.01 |  51.01
##     2 |                      Mujer | 743 | 45.44 |   45.44 |  96.45
##     3 |                       Otro |  33 |  2.02 |    2.02 |  98.47
##     4 | Marca mas de 1 alternativa |   3 |  0.18 |    0.18 |  98.65
##     9 |                         Nr |  22 |  1.35 |    1.35 | 100.00
##  <NA> |                       <NA> |   0 |  0.00 |    <NA> |   <NA>
```

##### Recodificación
Se transforman los valores 4 y 9, correspondientes a las categorías "Marca mas de 1 alternativa" y "Nr". Tammbién se pasa la variable a factor.


```r
# Genero estudiantes
est_proc$genero <- set_na(est_proc$genero, na = c(4,9), drop.levels = TRUE, as.tag = FALSE)
```

##### Etiquetado

```r
# Genero estudiantes
est_proc$genero <- set_label(x = est_proc$genero, label = "Sexo estudiante")
```

##### Descriptivo post-rec

```r
# Genero estudiantes
frq(est_proc$genero)
```

```
## 
## Sexo estudiante (x) <numeric>
## # total N=1635  valid N=1610  mean=1.50  sd=0.54
## 
## Value |  Label |   N | Raw % | Valid % | Cum. %
## -----------------------------------------------
##     1 | Hombre | 834 | 51.01 |   51.80 |  51.80
##     2 |  Mujer | 743 | 45.44 |   46.15 |  97.95
##     3 |   Otro |  33 |  2.02 |    2.05 | 100.00
##  <NA> |   <NA> |  25 |  1.53 |    <NA> |   <NA>
```

##### Otros ajustes
Crear variable factor


```r
# Transformación a factor
est_proc$genero_factor <- factor(est_proc$genero, levels = c(1,2,3), labels = c("Hombre", "Mujer", "Otro"))

# Etiquetar
est_proc$genero_factor <- set_label(x = est_proc$genero_factor, label = "Sexo estudiante")

# Crear variable Porcentaje de estudiantes mujeres en la escuela (N2)
porc_muj_esc <- est_proc %>%group_by(rbd)%>%summarise(porc_muj_esc = length(which(genero == 2))/(length(which(genero == 1)) + length(which(genero == 2)) + length(which(genero == 3))))

est_proc <- left_join(est_proc, porc_muj_esc, by = "rbd")
# Etiquetar
est_proc$porc_muj_esc <-  set_label(x = est_proc$porc_muj_esc, label = "Porcentaje de mujeres estudiantes por escuela")
```


#### Sentirse escuchado por profesor (N1)

##### Descriptivo

```r
frq(est_proc$resp_prof)
```

```
## 
## P56B ¿Cuán de acuerdo o en desacuerdo estás con las siguientes afirmaciones?EN G (x) <numeric>
## # total N=1635  valid N=1635  mean=3.36  sd=1.98
## 
## Value |                      Label |   N | Raw % | Valid % | Cum. %
## -------------------------------------------------------------------
##     1 |          Muy en desacuerdo | 113 |  6.91 |    6.91 |   6.91
##     2 |              En desacuerdo | 344 | 21.04 |   21.04 |  27.95
##     3 |                 De acuerdo | 797 | 48.75 |   48.75 |  76.70
##     4 |             Muy de acuerdo | 222 | 13.58 |   13.58 |  90.28
##     5 | Marca mas de 1 alternativa |   3 |  0.18 |    0.18 |  90.46
##     9 |                         Nr | 156 |  9.54 |    9.54 | 100.00
##  <NA> |                       <NA> |   0 |  0.00 |    <NA> |   <NA>
```

##### Etiquetado

```r
#Etiquetado  perdidos
est_proc$resp_prof <- set_na(est_proc$resp_prof, na = c(5,9), drop.levels = TRUE, as.tag = FALSE)
est_proc$resp_prof <- set_label(x = est_proc$resp_prof, label = "Siento que mi opinión es tomada en cuenta por mis profesores")
```

##### Descriptivo post-rec

```r
frq(est_proc$resp_prof)
```

```
## 
## Siento que mi opinión es tomada en cuenta por mis profesores (x) <numeric>
## # total N=1635  valid N=1476  mean=2.76  sd=0.80
## 
## Value |             Label |   N | Raw % | Valid % | Cum. %
## ----------------------------------------------------------
##     1 | Muy en desacuerdo | 113 |  6.91 |    7.66 |   7.66
##     2 |     En desacuerdo | 344 | 21.04 |   23.31 |  30.96
##     3 |        De acuerdo | 797 | 48.75 |   54.00 |  84.96
##     4 |    Muy de acuerdo | 222 | 13.58 |   15.04 | 100.00
##  <NA> |              <NA> | 159 |  9.72 |    <NA> |   <NA>
```

##### Otros ajustes

```r
# Transformación a factor
est_proc$resp_prof_factor <- factor(est_proc$resp_prof, levels = c(1,2,3,4), labels = c("Muy en desacuerdo", "En desacuerdo", "De acuerdo", "Muy de acuerdo"))
est_proc$resp_prof_factor <- set_label(x = est_proc$resp_prof_factor, label = "Justicia en el trato profesores")
```


#### Posición política (Control N1)
##### Descriptivo

```r
# Posición política estudiantes
frq(est_proc$pos_pol)
```

```
## 
## P59 Tradicionalmente la gente define las posiciones políticas como más cercanas  (x) <numeric>
## # total N=1635  valid N=1635  mean=7.48  sd=10.35
## 
## Value |                      Label |   N | Raw % | Valid % | Cum. %
## -------------------------------------------------------------------
##     1 |                    Derecha | 171 | 10.46 |   10.46 |  10.46
##     2 |             Centro derecha |  80 |  4.89 |    4.89 |  15.35
##     3 |                     Centro |  60 |  3.67 |    3.67 |  19.02
##     4 |           Centro Izquierda |  80 |  4.89 |    4.89 |  23.91
##     5 |                  Izquierda |  82 |  5.02 |    5.02 |  28.93
##     6 |              Independiente |  65 |  3.98 |    3.98 |  32.91
##     7 |                    Ninguna | 352 | 21.53 |   21.53 |  54.43
##     8 |                      No sé | 550 | 33.64 |   33.64 |  88.07
##     9 |                         Nr | 170 | 10.40 |   10.40 |  98.47
##    88 | Marca mas de 1 alternativa |  25 |  1.53 |    1.53 | 100.00
##  <NA> |                       <NA> |   0 |  0.00 |    <NA> |   <NA>
```

```r
summary(est_proc$pos_pol)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   5.000   7.000   7.477   8.000  88.000
```

##### Recodificación
Se recodifican a "Ns/Nr" los valores 8 (No sabe) y 9 (Nr). Los valores 88 (Marca más de una alternativa) se recodifican a NA 


```r
# Posición política estudiantes
est_proc$pos_pol[est_proc$pos_pol == 8 | est_proc$pos_pol == 9] <- 99
est_proc$pos_pol <- set_labels(est_proc$pos_pol,
            labels=c("Right" = 1,
                     "Center Right" = 2,
                     "Center" = 3,
                     "Center Left" = 4,
                     "Left" = 5,
                     "Independent" = 6,
                     "None" = 7,
                     "Na/Nr"=99)) #Etiquetado categorías

est_proc$pos_pol<- set_na(est_proc$pos_pol, na = c(88), drop.levels = TRUE, as.tag = FALSE)
```

##### Etiquetado

```r
# Posición política estudiantes
est_proc$pos_pol <- set_label(x = est_proc$pos_pol, label = "Political position students")
```

##### Descriptivo post-rec

```r
# Posición política estudiantes
frq(est_proc$pos_pol)
```

```
## 
## Political position students (x) <numeric>
## # total N=1635  valid N=1610  mean=46.82  sd=46.98
## 
## Value |        Label |   N | Raw % | Valid % | Cum. %
## -----------------------------------------------------
##     1 |        Right | 171 | 10.46 |   10.62 |  10.62
##     2 | Center Right |  80 |  4.89 |    4.97 |  15.59
##     3 |       Center |  60 |  3.67 |    3.73 |  19.32
##     4 |  Center Left |  80 |  4.89 |    4.97 |  24.29
##     5 |         Left |  82 |  5.02 |    5.09 |  29.38
##     6 |  Independent |  65 |  3.98 |    4.04 |  33.42
##     7 |         None | 352 | 21.53 |   21.86 |  55.28
##    99 |        Na/Nr | 720 | 44.04 |   44.72 | 100.00
##  <NA> |         <NA> |  25 |  1.53 |    <NA> |   <NA>
```

```r
summary(est_proc$pos_pol)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    1.00    5.00    7.00   46.82   99.00   99.00      25
```

##### Otros ajustes
Crear variable factor

```r
# Transformación a factor
est_proc$pos_pol_factor <- factor(est_proc$pos_pol, levels = c(1,2,3,4,5,6,7,99), labels = c("Right", "Center Right", "Center", "Center Left", "Left", "Independent", "None", "Na/Nr"))

# Etiquetar
est_proc$pos_pol_factor <- set_label(x = est_proc$pos_pol_factor, label = "Political position students")
```

#### Cantidad de libros en el hogar (Control N1)
##### Descriptivo

```r
# Cuestionario estudiantes
frq(est_proc$libros_hogar)
```

```
## 
## P68 Aproximadamente ¿Cuántos libros hay en tu hogar? (x) <numeric>
## # total N=1635  valid N=1635  mean=3.29  sd=2.21
## 
## Value |                      Label |   N | Raw % | Valid % | Cum. %
## -------------------------------------------------------------------
##     1 |        Entre 0 y 10 libros | 330 | 20.18 |   20.18 |  20.18
##     2 |       Entre 11 y 25 libros | 348 | 21.28 |   21.28 |  41.47
##     3 |      Entre 26 y 100 libros | 434 | 26.54 |   26.54 |  68.01
##     4 |     Entre 101 y 200 libros | 201 | 12.29 |   12.29 |  80.31
##     5 |     Entre 201 y 500 libros | 113 |  6.91 |    6.91 |  87.22
##     6 |          Más de 500 libros |  62 |  3.79 |    3.79 |  91.01
##     8 | Marca mas de 1 alternativa |   5 |  0.31 |    0.31 |  91.31
##     9 |                         Nr | 142 |  8.69 |    8.69 | 100.00
##  <NA> |                       <NA> |   0 |  0.00 |    <NA> |   <NA>
```

##### Recodificación
Se transforma el valor 8 (Marca mas de 1 alternativa) y el valor 9 (Nr) en NA.


```r
#Cuestionario estudiantes
est_proc$libros_hogar <- set_na(est_proc$libros_hogar, na = c(8,9), drop.levels = TRUE, as.tag = FALSE)
```

##### Etiquetado

```r
# Cuestionaro estudiantes 
est_proc$libros_hogar <- set_label(x = est_proc$libros_hogar, label = "Books in household")
```

##### Descriptivo

```r
# Cuestionario estudiantes
frq(est_proc$libros_hogar)
```

```
## 
## Books in household (x) <numeric>
## # total N=1635  valid N=1488  mean=2.73  sd=1.37
## 
## Value |                  Label |   N | Raw % | Valid % | Cum. %
## ---------------------------------------------------------------
##     1 |    Entre 0 y 10 libros | 330 | 20.18 |   22.18 |  22.18
##     2 |   Entre 11 y 25 libros | 348 | 21.28 |   23.39 |  45.56
##     3 |  Entre 26 y 100 libros | 434 | 26.54 |   29.17 |  74.73
##     4 | Entre 101 y 200 libros | 201 | 12.29 |   13.51 |  88.24
##     5 | Entre 201 y 500 libros | 113 |  6.91 |    7.59 |  95.83
##     6 |      Más de 500 libros |  62 |  3.79 |    4.17 | 100.00
##  <NA> |                   <NA> | 147 |  8.99 |    <NA> |   <NA>
```

##### Otros ajustes 
Crear variable factor


```r
# Transformación a factor
est_proc$libros_hogar_factor <- factor(est_proc$libros_hogar, levels = c(1,2,3,4,5,6), labels = c("Between 0 and 10 books", "Between 11 and 25 books", "Between 26 and 100 book", "Between 101 and 200 books", "Between 201 and 500 books", "More than 500 books"))

# Etiquetar
est_proc$libros_hogar_factor <- set_label(x = est_proc$libros_hogar_factor, label = "Books in household")
```


#### Region (Control N1)
##### Descriptivo

```r
# Cuestionario estudiantes
frq(est_proc$region)
```

```
## 
## Región (x) <numeric>
## # total N=1635  valid N=1635  mean=8.96  sd=4.65
## 
## Value |                 Label |   N | Raw % | Valid % | Cum. %
## --------------------------------------------------------------
##     2 | Región de Antofagasta | 407 | 24.89 |   24.89 |  24.89
##     7 |      Región del Maule | 354 | 21.65 |   21.65 |  46.54
##    13 |  Región Metropolitana | 874 | 53.46 |   53.46 | 100.00
##  <NA> |                  <NA> |   0 |  0.00 |    <NA> |   <NA>
```

##### Recodificación
No aplica.

##### Etiquetado

```r
# Cuestionario estudiantes
est_proc$region <- set_label(x = est_proc$region, label = "Region estudiantes")
```

##### Descriptivo post-rec
No aplica.

##### Otros ajustes
Crear variable factor


```r
# Transformación a factor
est_proc$region_factor <- factor(est_proc$region, levels = c(2,7,13), labels = c("Región de Antofagasta", "Región del Maule", "Región Metropolitana"))

# Etiquetar
est_proc$region_factor <- set_label(est_proc$region_factor, label = "Region estudiantes factor")
```


#### Recompensa (Control N1)
##### Descriptivo 

```r
# Cuestionario estudiante
frq(est_proc$recompensa)
```

```
## 
## P25C ¿Cuán de acuerdo o en desacuerdo estás con las siguientes afirmaciones?EN E (x) <numeric>
## # total N=1635  valid N=1635  mean=2.97  sd=1.49
## 
## Value |                      Label |   N | Raw % | Valid % | Cum. %
## -------------------------------------------------------------------
##     1 |          Muy en desacuerdo | 159 |  9.72 |    9.72 |   9.72
##     2 |              En desacuerdo | 373 | 22.81 |   22.81 |  32.54
##     3 |                 De acuerdo | 782 | 47.83 |   47.83 |  80.37
##     4 |             Muy de acuerdo | 254 | 15.54 |   15.54 |  95.90
##     5 | Marca mas de 1 alternativa |   2 |  0.12 |    0.12 |  96.02
##     9 |                         Nr |  65 |  3.98 |    3.98 | 100.00
##  <NA> |                       <NA> |   0 |  0.00 |    <NA> |   <NA>
```

##### Recodificacion.
Se transforma el valor 5 (Marca mas de una alternativa) y 9 (Nr) a NA.


```r
# Cuestionario estudiantes
est_proc$recompensa <- set_na(est_proc$recompensa, na = c(5,9), drop.levels = TRUE, as.tag = FALSE)
```

##### Etiquetado

```r
# Cuestionario estudiantes
est_proc$recompensa <- set_label(x = est_proc$recompensa, label = "Esfuerzo es recompensado en escuela (estudiantes)")
```

##### Descriptivo post-rec

```r
# Cuestionario estudiante
frq(est_proc$recompensa)
```

```
## 
## Esfuerzo es recompensado en escuela (estudiantes) (x) <numeric>
## # total N=1635  valid N=1568  mean=2.72  sd=0.85
## 
## Value |             Label |   N | Raw % | Valid % | Cum. %
## ----------------------------------------------------------
##     1 | Muy en desacuerdo | 159 |  9.72 |   10.14 |  10.14
##     2 |     En desacuerdo | 373 | 22.81 |   23.79 |  33.93
##     3 |        De acuerdo | 782 | 47.83 |   49.87 |  83.80
##     4 |    Muy de acuerdo | 254 | 15.54 |   16.20 | 100.00
##  <NA> |              <NA> |  67 |  4.10 |    <NA> |   <NA>
```

##### Otros ajustes
Crear variable factor


```r
# Transformación a factor
est_proc$recompensa_factor <- factor(est_proc$recompensa, levels = c(1,2,3,4), labels = c("Muy en desacuerdo", "En desacuerdo", "De acuerdo", "Muy de acuerdo"))

# Etiquetar
est_proc$recompensa_factor <- set_label(est_proc$recompensa_factor, label = "Esfuerzo es recompensado en escuela (estudiantes)")
```


#### Dependencia administrativa escuela (Control N2)
##### Descriptivo

```r
# Cuestionario estudiantes
frq(est_proc$dependencia)
```

```
## 
## Dependencia (x) <numeric>
## # total N=1635  valid N=1635  mean=1.78  sd=0.69
## 
## Value |               Label |   N | Raw % | Valid % | Cum. %
## ------------------------------------------------------------
##     1 |           Municipal | 605 | 37.00 |   37.00 |  37.00
##     2 | Part. Subvencionado | 780 | 47.71 |   47.71 |  84.71
##     3 |        Part.Privado | 250 | 15.29 |   15.29 | 100.00
##  <NA> |                <NA> |   0 |  0.00 |    <NA> |   <NA>
```

```r
summary(est_proc$dependencia)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   1.000   2.000   1.783   2.000   3.000
```

##### Recodificación
No aplica.

##### Etiquetado

```r
# Etiquetar
est_proc$dependencia <- set_label(est_proc$dependencia, label = "Dependencia estudiantes")
```


##### Descriptivo post-rec
No aplica.

##### Otros ajustes
Crear variable factor


```r
# Transformación a factor
est_proc$dependencia_factor <- factor(est_proc$dependencia, levels = c(1,2,3), labels = c("Municipal", "Particular Subvencionado", "Particular Pagado"))

# Etiquetar
est_proc$dependencia_factor <- set_label(est_proc$dependencia_factor, label = "Dependencia estudiantes factor")
```


### Variables apoderados

#### Percepción meritocratica apoderados: Importancia trabajo duro (Var. Indep. N1)
##### Descriptivo

```r
frq(ap_proc$perc_trabajo_duro)
```

```
## 
## P9D Actualmente en Chile, para surgir en la vida ¿Cuán importante esEL TRABAJO D (x) <numeric>
## # total N=744  valid N=744  mean=3.45  sd=1.43
## 
## Value |           Label |   N | Raw % | Valid % | Cum. %
## --------------------------------------------------------
##     1 | Nada importante |  49 |  6.59 |    6.59 |   6.59
##     2 | Algo importante |  89 | 11.96 |   11.96 |  18.55
##     3 |      Importante | 226 | 30.38 |   30.38 |  48.92
##     4 |  Muy importante | 351 | 47.18 |   47.18 |  96.10
##     9 |              Nr |  29 |  3.90 |    3.90 | 100.00
##  <NA> |            <NA> |   0 |  0.00 |    <NA> |   <NA>
```

##### Recodificación
Se recodifica a NA la categoría 9, correspondiente a la etiquetas "Nr".


```r
ap_proc$perc_trabajo_duro <- set_na(ap_proc$perc_trabajo_duro, na = c(9), drop.levels = TRUE, as.tag = FALSE)
```

##### Etiquetado

```r
ap_proc$perc_trabajo_duro <- set_label(x = ap_proc$perc_trabajo_duro,label = "Percepción meritocracia: Importancia trabajo duro apoderados")
```

##### Descriptivo post-rec

```r
frq(ap_proc$perc_trabajo_duro)
```

```
## 
## Percepción meritocracia: Importancia trabajo duro apoderados (x) <numeric>
## # total N=744  valid N=715  mean=3.23  sd=0.92
## 
## Value |           Label |   N | Raw % | Valid % | Cum. %
## --------------------------------------------------------
##     1 | Nada importante |  49 |  6.59 |    6.85 |   6.85
##     2 | Algo importante |  89 | 11.96 |   12.45 |  19.30
##     3 |      Importante | 226 | 30.38 |   31.61 |  50.91
##     4 |  Muy importante | 351 | 47.18 |   49.09 | 100.00
##  <NA> |            <NA> |  29 |  3.90 |    <NA> |   <NA>
```

##### Otros ajustes
Crear variable factor

```r
# Transformación a factor
ap_proc$perc_trabajo_duro_factor <- factor(ap_proc$perc_trabajo_duro, levels = c(1,2,3,4), labels = c("Not important", "Somewhat important", "Important", "Very important"))

# Etiquetar
ap_proc$perc_trabajo_duro_factor <- set_label(x = ap_proc$perc_trabajo_duro_factor, label = "Perception of meritocracy: Importance of hard work (parents)")
```


#### Percepción meritocratica apoderados: Esfuerzo para salir adelante (Var. Indep. N1)

##### Descriptivo

```r
frq(ap_proc$perc_esfuerzo)
```

```
## 
## P10D ¿Cuán de acuerdo o en desacuerdo está usted con las siguientes afirmaciones (x) <numeric>
## # total N=744  valid N=744  mean=3.41  sd=1.44
## 
## Value |             Label |   N | Raw % | Valid % | Cum. %
## ----------------------------------------------------------
##     1 | Muy en desacuerdo |  24 |  3.23 |    3.23 |   3.23
##     2 |     En desacuerdo | 102 | 13.71 |   13.71 |  16.94
##     3 |        De acuerdo | 331 | 44.49 |   44.49 |  61.42
##     4 |    Muy de acuerdo | 253 | 34.01 |   34.01 |  95.43
##     9 |                Nr |  34 |  4.57 |    4.57 | 100.00
##  <NA> |              <NA> |   0 |  0.00 |    <NA> |   <NA>
```

##### Recodificación
Se recodifica a NA la categoría 9, correspondiente a la etiqueta "Nr".


```r
ap_proc$perc_esfuerzo <- set_na(ap_proc$perc_esfuerzo, na = c(9), drop.levels = TRUE, as.tag = FALSE)
```

##### Etiquetado

```r
ap_proc$perc_esfuerzo <- set_label(x = ap_proc$perc_esfuerzo, label = "Percepción meritocracia: Esfuerzo para salir adelante apoderados")
```

##### Descriptivo post-rec

```r
frq(ap_proc$perc_esfuerzo)
```

```
## 
## Percepción meritocracia: Esfuerzo para salir adelante apoderados (x) <numeric>
## # total N=744  valid N=710  mean=3.15  sd=0.78
## 
## Value |             Label |   N | Raw % | Valid % | Cum. %
## ----------------------------------------------------------
##     1 | Muy en desacuerdo |  24 |  3.23 |    3.38 |   3.38
##     2 |     En desacuerdo | 102 | 13.71 |   14.37 |  17.75
##     3 |        De acuerdo | 331 | 44.49 |   46.62 |  64.37
##     4 |    Muy de acuerdo | 253 | 34.01 |   35.63 | 100.00
##  <NA> |              <NA> |  34 |  4.57 |    <NA> |   <NA>
```

##### Otros ajustes
Crear variable factor

```r
# Transformación a factor
ap_proc$perc_esfuerzo_factor <- factor(ap_proc$perc_esfuerzo, levels = c(1,2,3,4), labels = c("Strongly disagree","Disagree", "Agree", "Strongly agree"))

# Etiquetar
ap_proc$perc_esfuerzo_factor <- set_label(x = ap_proc$perc_esfuerzo_factor, label = "Perception of meritocracy: Effort to get ahead  (parents)")
```

Creación variable: promedio de percepción de meritocracia en apoderados

```r
# Crear variable
ap_proc$prom_percmer_ap <- ((ap_proc$perc_trabajo_duro + ap_proc$perc_esfuerzo)/2)

# Etiquetar
ap_proc$prom_percmer_ap <- set_label(x = ap_proc$prom_percmer_ap, label = "Promedio Percepción meritocratica apoderados")
```


#### NSE: Ingreso (Var. Indep. N1)

##### Descriptivo

```r
frq(ap_proc$ingresos_tramos)
```

```
## 
## P55 A continuación, le presentamos un listado de rangos de ingreso. ¿Podría uste (x) <numeric>
## # total N=744  valid N=744  mean=8.27  sd=3.05
## 
## Value |                                         Label |   N | Raw % | Valid % | Cum. %
## --------------------------------------------------------------------------------------
##     1 |          Menos de $101.000 mensuales líquidos |  13 |  1.75 |    1.75 |   1.75
##     2 |     De $101.001 a $134.000 mensuales líquidos |  15 |  2.02 |    2.02 |   3.76
##     3 |     De $134.001 a $179.000 mensuales líquidos |  13 |  1.75 |    1.75 |   5.51
##     4 |     De $179.001 a $224.000 mensuales líquidos |  30 |  4.03 |    4.03 |   9.54
##     5 |     De $224.001 a $291.000 mensuales líquidos |  40 |  5.38 |    5.38 |  14.92
##     6 |     De $291.001 a $358.000 mensuales líquidos |  91 | 12.23 |   12.23 |  27.15
##     7 |     De $358.001 a $448.000 mensuales líquidos | 110 | 14.78 |   14.78 |  41.94
##     8 |   De $448.001 a $1.000.000 mensuales líquidos | 141 | 18.95 |   18.95 |  60.89
##     9 | De $1.000.001 a $2.000.000 mensuales líquidos |  81 | 10.89 |   10.89 |  71.77
##    10 | De $2.000.001 a $3.000.000 mensuales líquidos |  18 |  2.42 |    2.42 |  74.19
##    11 |          Más de $3.000.000 mensuales líquidos |  44 |  5.91 |    5.91 |  80.11
##    12 |                                       No sabe |  12 |  1.61 |    1.61 |  81.72
##    13 |                                   No responde | 136 | 18.28 |   18.28 | 100.00
##  <NA> |                                          <NA> |   0 |  0.00 |    <NA> |   <NA>
```

##### Recodificación
Se juntan los números 12 y 13, correspondiente a las categorías "No sabe" y "No responde", respectivamente.

```r
# Ingresos tramos
ap_proc$ingresos_tramos[ap_proc$ingresos_tramos == 12 | ap_proc$ingresos_tramos == 13] <- 99

# Etiquetar
ap_proc$ingresos_tramos <- set_labels(x = ap_proc$ingresos_tramos, labels = c("Menos de $101.000 mensuales líquidos" = 1,
                                                                            "De $101.001 a $134.000 mensuales líquidos" = 2,
                                                                            "De $134.001 a $179.000 mensuales líquidos" = 3,
                                                                            "De $179.001 a $224.000 mensuales líquidos" = 4,
                                                                            "De $224.001 a $291.000 mensuales líquidos" = 5,
                                                                            "De $291.001 a $358.000 mensuales líquidos" = 6,
                                                                            "De $358.001 a $448.000 mensuales líquidos" = 7,
                                                                            "De $448.001 a $1.000.000 mensuales líquidos" = 8,
                                                                            "De $1.000.001 a $2.000.000 mensuales líquidos" = 9,
                                                                            "De $2.000.001 a $3.000.000 mensuales líquidos" = 10,
                                                                            "Más de $3.000.000 mensuales líquidos" = 11,
                                                                            "Ns/Nr" = 99))
# Drop levels
ap_proc$ingresos_tramos <- set_na(ap_proc$ingresos_tramos, na = c(12,13), drop.levels = TRUE, as.tag = FALSE)
```

##### Etiquetado

```r
ap_proc$ingresos_tramos <- set_label(x = ap_proc$ingresos_tramos,label = "Ingresos del hogar en tramos")
```

##### Descriptivo post-rec

```r
frq(ap_proc$ingresos_tramos)
```

```
## 
## Ingresos del hogar en tramos (x) <numeric>
## # total N=744  valid N=744  mean=25.39  sd=36.76
## 
## Value |                                         Label |   N | Raw % | Valid % | Cum. %
## --------------------------------------------------------------------------------------
##     1 |          Menos de $101.000 mensuales líquidos |  13 |  1.75 |    1.75 |   1.75
##     2 |     De $101.001 a $134.000 mensuales líquidos |  15 |  2.02 |    2.02 |   3.76
##     3 |     De $134.001 a $179.000 mensuales líquidos |  13 |  1.75 |    1.75 |   5.51
##     4 |     De $179.001 a $224.000 mensuales líquidos |  30 |  4.03 |    4.03 |   9.54
##     5 |     De $224.001 a $291.000 mensuales líquidos |  40 |  5.38 |    5.38 |  14.92
##     6 |     De $291.001 a $358.000 mensuales líquidos |  91 | 12.23 |   12.23 |  27.15
##     7 |     De $358.001 a $448.000 mensuales líquidos | 110 | 14.78 |   14.78 |  41.94
##     8 |   De $448.001 a $1.000.000 mensuales líquidos | 141 | 18.95 |   18.95 |  60.89
##     9 | De $1.000.001 a $2.000.000 mensuales líquidos |  81 | 10.89 |   10.89 |  71.77
##    10 | De $2.000.001 a $3.000.000 mensuales líquidos |  18 |  2.42 |    2.42 |  74.19
##    11 |          Más de $3.000.000 mensuales líquidos |  44 |  5.91 |    5.91 |  80.11
##    99 |                                         Ns/Nr | 148 | 19.89 |   19.89 | 100.00
##  <NA> |                                          <NA> |   0 |  0.00 |    <NA> |   <NA>
```

##### Otros ajustes

Se construyen cuatro variables nuevas. La primera es el ingreso a partir de las marcas de clase de los tramos de ingreso. La segunda es el ingreso per capita. La tercera se construye una variable en tramos tipo factor y la cuarta serán los quintiles de ingreso per capita en su version numerica y factor.


```r
# Construcción ingresos numerico


ap_proc$ingresos[ap_proc$ingresos_tramos == 1] <- 50500
ap_proc$ingresos[ap_proc$ingresos_tramos == 2] <- 117500
ap_proc$ingresos[ap_proc$ingresos_tramos == 3] <- 156500
ap_proc$ingresos[ap_proc$ingresos_tramos == 4] <- 201500
ap_proc$ingresos[ap_proc$ingresos_tramos == 5] <- 257500
ap_proc$ingresos[ap_proc$ingresos_tramos == 6] <- 324500
ap_proc$ingresos[ap_proc$ingresos_tramos == 7] <- 403000
ap_proc$ingresos[ap_proc$ingresos_tramos == 8] <- 724000
ap_proc$ingresos[ap_proc$ingresos_tramos == 9] <- 1500000
ap_proc$ingresos[ap_proc$ingresos_tramos == 10] <- 2500000
ap_proc$ingresos[ap_proc$ingresos_tramos == 11] <- 3500000

# Construcción ingreso per capita
ap_proc$ingresos_pc <- ap_proc$ingresos/ap_proc$personas_hogar
ap_proc$ingresos_pc <- trunc(ap_proc$ingresos_pc)

# Construcción ingreso tramos como factor
ap_proc$ingresos_tramos_factor <- factor(ap_proc$ingresos_tramos, 
                                         levels = c(1,2,3,4,5,6,7,8,9,10,11,99), 
                                         labels = c("Menos de $101.000 mensuales líquidos",
                                                    "De $101.001 a $134.000 mensuales líquidos",
                                                    "De $134.001 a $179.000 mensuales líquidos",
                                                    "De $179.001 a $224.000 mensuales líquidos",
                                                    "De $224.001 a $291.000 mensuales líquidos",
                                                    "De $291.001 a $358.000 mensuales líquidos",
                                                    "De $358.001 a $448.000 mensuales líquidos",
                                                    "De $448.001 a $1.000.000 mensuales líquidos",
                                                    "De $1.000.001 a $2.000.000 mensuales líquidos",
                                                    "De $2.000.001 a $3.000.000 mensuales líquidos",
                                                    "Más de $3.000.000 mensuales líquidos",
                                                    "Ns/Nr"))

# Construccion ingresos per capita quintiles
ap_proc <- ap_proc %>% mutate(quintiles_ingresos_pc = ntile(ingresos_pc,5))

## Recuperar NA
ap_proc$quintiles_ingresos_pc[is.na(ap_proc$quintiles_ingresos_pc)] <- 99

# Construcción ingresos per capita quintiles factor
ap_proc$quintiles_ingresos_pc_factor <- factor(ap_proc$quintiles_ingresos_pc, levels = c(1,2,3,4,5,99), labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5", "Don't know/No response"))
ap_proc$quintiles_ingresos_pc_factor <- set_label(ap_proc$quintiles_ingresos_pc_factor,label = "Household income ")

# Construcción ingresos per capita quintiles factor con NS/NR al medio para correlaciones
ap_proc$quintiles_ingresos_pc_factor_cor <- factor(ap_proc$quintiles_ingresos_pc, levels = c(1,2,99,3,4,5), labels = c("Quintil 1", "Quintil 2", "Quintil 3", "No sabe/No responde", "Quintil 4", "Quintil 5"))
ap_proc$quintiles_ingresos_pc_factor_cor <- set_label(ap_proc$quintiles_ingresos_pc_factor,label = "Quintiles de ingreso per capita del hogar (NS/NR al medio) ")

## Eliminar 99 de la variable numerica
ap_proc$quintiles_ingresos_pc[ap_proc$quintiles_ingresos_pc == 99] <- NA
ap_proc$quintiles_ingresos_pc <- set_label(ap_proc$quintiles_ingresos_pc,label = "Quintiles de ingreso per capita del hogar (numerica)") 

# ingresos
summary(ap_proc$ingresos)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   50500  324500  403000  867856  724000 3500000     148
```

```r
# ingresos per capita
summary(ap_proc$ingresos_pc)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##     201   67166  124708  207972  241333 1750000     148
```

```r
# ingresos tramos factor
frq(ap_proc$ingresos_tramos_factor)
```

```
## 
## x <categorical>
## # total N=744  valid N=744  mean=8.08  sd=2.78
## 
## Value                                         |   N | Raw % | Valid % | Cum. %
## ------------------------------------------------------------------------------
## Menos de $101.000 mensuales líquidos          |  13 |  1.75 |    1.75 |   1.75
## De $101.001 a $134.000 mensuales líquidos     |  15 |  2.02 |    2.02 |   3.76
## De $134.001 a $179.000 mensuales líquidos     |  13 |  1.75 |    1.75 |   5.51
## De $179.001 a $224.000 mensuales líquidos     |  30 |  4.03 |    4.03 |   9.54
## De $224.001 a $291.000 mensuales líquidos     |  40 |  5.38 |    5.38 |  14.92
## De $291.001 a $358.000 mensuales líquidos     |  91 | 12.23 |   12.23 |  27.15
## De $358.001 a $448.000 mensuales líquidos     | 110 | 14.78 |   14.78 |  41.94
## De $448.001 a $1.000.000 mensuales líquidos   | 141 | 18.95 |   18.95 |  60.89
## De $1.000.001 a $2.000.000 mensuales líquidos |  81 | 10.89 |   10.89 |  71.77
## De $2.000.001 a $3.000.000 mensuales líquidos |  18 |  2.42 |    2.42 |  74.19
## Más de $3.000.000 mensuales líquidos          |  44 |  5.91 |    5.91 |  80.11
## Ns/Nr                                         | 148 | 19.89 |   19.89 | 100.00
## <NA>                                          |   0 |  0.00 |    <NA> |   <NA>
```

```r
#---- Etiquetado variables nuevas ----
#ingreso
ap_proc$ingresos <- set_label(x = ap_proc$ingresos, label = "Ingresos del hogar")

# ingreso per capita
ap_proc$ingresos_pc <- set_label(x = ap_proc$ingresos_pc, label = "Ingreso per capita")

# ingreso factor
ap_proc$ingresos_tramos_factor <- set_label(x = ap_proc$ingresos_tramos_factor, label = "Ingresos del hogar en tramos factor")
```


#### NSE: Educación (Var. Indep. N1)
##### Descriptivo

```r
# Cuestionario apoderados
frq(ap_proc$educ)
```

```
## 
## P45 ¿Cuál es el último curso o nivel de estudios que completó usted? (x) <numeric>
## # total N=744  valid N=744  mean=2.85  sd=1.67
## 
## Value |                                                                        Label |   N | Raw % | Valid % | Cum. %
## ---------------------------------------------------------------------------------------------------------------------
##     1 |                                                           8vo básico o menos |  79 | 10.62 |   10.62 |  10.62
##     2 |                                                              Educación Media | 312 | 41.94 |   41.94 |  52.55
##     3 | Educación Técnica Superior (Instituto Profesional o Centro de Formación Técn | 183 | 24.60 |   24.60 |  77.15
##     4 |  Una carrera en la Universidad o estudios de Posgrado (Magíster o Doctorado) | 133 | 17.88 |   17.88 |  95.03
##     9 |                                                                           Nr |  37 |  4.97 |    4.97 | 100.00
##  <NA> |                                                                         <NA> |   0 |  0.00 |    <NA> |   <NA>
```

##### Recodificacion
Se transforma a NA el valor 9 (Nr).


```r
# Cuestionario apoderados
ap_proc$educ <- set_na(ap_proc$educ, na = c(9), drop.levels = TRUE, as.tag = FALSE)
```

##### Etiquetado

```r
# Cuestionario apoderados
ap_proc$educ <- set_label(x = ap_proc$educ, label = "Educacion del apoderado")
```

##### Descriptivo post-rec

```r
# Cuestionario apoderados
frq(ap_proc$educ)
```

```
## 
## Educacion del apoderado (x) <numeric>
## # total N=744  valid N=707  mean=2.52  sd=0.92
## 
## Value |                                                                        Label |   N | Raw % | Valid % | Cum. %
## ---------------------------------------------------------------------------------------------------------------------
##     1 |                                                           8vo básico o menos |  79 | 10.62 |   11.17 |  11.17
##     2 |                                                              Educación Media | 312 | 41.94 |   44.13 |  55.30
##     3 | Educación Técnica Superior (Instituto Profesional o Centro de Formación Técn | 183 | 24.60 |   25.88 |  81.19
##     4 |  Una carrera en la Universidad o estudios de Posgrado (Magíster o Doctorado) | 133 | 17.88 |   18.81 | 100.00
##  <NA> |                                                                         <NA> |  37 |  4.97 |    <NA> |   <NA>
```

##### Otros ajustes
Al final del documento, después de combinar las bases.


#### Edad (Var. Indep. N1)
##### Descriptivo

```r
# Edad apoderados
frq(ap_proc$nacimiento)
```

```
## 
## P40A ¿Podría indicar su mes y año de nacimiento?AÑO (x) <numeric>
## # total N=744  valid N=744  mean=2902.13  sd=2567.42
## 
## Value | Label |  N | Raw % | Valid % | Cum. %
## ---------------------------------------------
##  1933 |  1933 |  1 |  0.13 |    0.13 |   0.13
##  1949 |  1949 |  1 |  0.13 |    0.13 |   0.27
##  1951 |  1951 |  1 |  0.13 |    0.13 |   0.40
##  1952 |  1952 |  2 |  0.27 |    0.27 |   0.67
##  1954 |  1954 |  1 |  0.13 |    0.13 |   0.81
##  1955 |  1955 |  2 |  0.27 |    0.27 |   1.08
##  1956 |  1956 |  2 |  0.27 |    0.27 |   1.34
##  1957 |  1957 |  4 |  0.54 |    0.54 |   1.88
##  1958 |  1958 |  3 |  0.40 |    0.40 |   2.28
##  1959 |  1959 |  5 |  0.67 |    0.67 |   2.96
##  1960 |  1960 |  4 |  0.54 |    0.54 |   3.49
##  1961 |  1961 |  5 |  0.67 |    0.67 |   4.17
##  1962 |  1962 |  3 |  0.40 |    0.40 |   4.57
##  1963 |  1963 | 15 |  2.02 |    2.02 |   6.59
##  1964 |  1964 | 10 |  1.34 |    1.34 |   7.93
##  1965 |  1965 | 11 |  1.48 |    1.48 |   9.41
##  1966 |  1966 | 16 |  2.15 |    2.15 |  11.56
##  1967 |  1967 | 20 |  2.69 |    2.69 |  14.25
##  1968 |  1968 | 25 |  3.36 |    3.36 |  17.61
##  1969 |  1969 | 24 |  3.23 |    3.23 |  20.83
##  1970 |  1970 | 39 |  5.24 |    5.24 |  26.08
##  1971 |  1971 | 40 |  5.38 |    5.38 |  31.45
##  1972 |  1972 | 40 |  5.38 |    5.38 |  36.83
##  1973 |  1973 | 31 |  4.17 |    4.17 |  40.99
##  1974 |  1974 | 36 |  4.84 |    4.84 |  45.83
##  1975 |  1975 | 21 |  2.82 |    2.82 |  48.66
##  1976 |  1976 | 24 |  3.23 |    3.23 |  51.88
##  1977 |  1977 | 30 |  4.03 |    4.03 |  55.91
##  1978 |  1978 | 31 |  4.17 |    4.17 |  60.08
##  1979 |  1979 | 21 |  2.82 |    2.82 |  62.90
##  1980 |  1980 | 39 |  5.24 |    5.24 |  68.15
##  1981 |  1981 | 35 |  4.70 |    4.70 |  72.85
##  1982 |  1982 | 31 |  4.17 |    4.17 |  77.02
##  1983 |  1983 | 14 |  1.88 |    1.88 |  78.90
##  1984 |  1984 | 15 |  2.02 |    2.02 |  80.91
##  1985 |  1985 | 14 |  1.88 |    1.88 |  82.80
##  1986 |  1986 | 12 |  1.61 |    1.61 |  84.41
##  1987 |  1987 |  6 |  0.81 |    0.81 |  85.22
##  1988 |  1988 |  5 |  0.67 |    0.67 |  85.89
##  1989 |  1989 |  4 |  0.54 |    0.54 |  86.42
##  1990 |  1990 |  1 |  0.13 |    0.13 |  86.56
##  1991 |  1991 |  2 |  0.27 |    0.27 |  86.83
##  1992 |  1992 |  2 |  0.27 |    0.27 |  87.10
##  1994 |  1994 |  1 |  0.13 |    0.13 |  87.23
##  1997 |  1997 |  1 |  0.13 |    0.13 |  87.37
##  1998 |  1998 |  2 |  0.27 |    0.27 |  87.63
##  2000 |  2000 |  1 |  0.13 |    0.13 |  87.77
##  2001 |  2001 |  1 |  0.13 |    0.13 |  87.90
##  2002 |  2002 |  3 |  0.40 |    0.40 |  88.31
##  2003 |  2003 |  1 |  0.13 |    0.13 |  88.44
##  9999 |    Nr | 86 | 11.56 |   11.56 | 100.00
##  <NA> |  <NA> |  0 |  0.00 |    <NA> |   <NA>
```

##### Recodificación
Se transforman a NA los valores 9999, correspondientes a la categoría "Nr". Provisoriamente, se transforman los años posteriores a 1992 a NA. Como referencia, un apoderado nacido en 1992 tendría 27 años al momento que su hijo tenga 16.


```r
ap_proc$nacimiento[ap_proc$nacimiento == 9999] <- NA
ap_proc$nacimiento[ap_proc$nacimiento > 1992] <- NA
```

##### Etiquetado

```r
ap_proc$nacimiento <- set_label(x = ap_proc$nacimiento, label = "Nacimiento apoderado")
```

##### Descriptivo post-rec

```r
# Edad apoderados
frq(ap_proc$nacimiento)
```

```
## 
## Nacimiento apoderado (x) <numeric>
## # total N=744  valid N=648  mean=1974.18  sd=7.46
## 
## Value | Label |  N | Raw % | Valid % | Cum. %
## ---------------------------------------------
##  1933 |  1933 |  1 |  0.13 |    0.15 |   0.15
##  1949 |  1949 |  1 |  0.13 |    0.15 |   0.31
##  1951 |  1951 |  1 |  0.13 |    0.15 |   0.46
##  1952 |  1952 |  2 |  0.27 |    0.31 |   0.77
##  1954 |  1954 |  1 |  0.13 |    0.15 |   0.93
##  1955 |  1955 |  2 |  0.27 |    0.31 |   1.23
##  1956 |  1956 |  2 |  0.27 |    0.31 |   1.54
##  1957 |  1957 |  4 |  0.54 |    0.62 |   2.16
##  1958 |  1958 |  3 |  0.40 |    0.46 |   2.62
##  1959 |  1959 |  5 |  0.67 |    0.77 |   3.40
##  1960 |  1960 |  4 |  0.54 |    0.62 |   4.01
##  1961 |  1961 |  5 |  0.67 |    0.77 |   4.78
##  1962 |  1962 |  3 |  0.40 |    0.46 |   5.25
##  1963 |  1963 | 15 |  2.02 |    2.31 |   7.56
##  1964 |  1964 | 10 |  1.34 |    1.54 |   9.10
##  1965 |  1965 | 11 |  1.48 |    1.70 |  10.80
##  1966 |  1966 | 16 |  2.15 |    2.47 |  13.27
##  1967 |  1967 | 20 |  2.69 |    3.09 |  16.36
##  1968 |  1968 | 25 |  3.36 |    3.86 |  20.22
##  1969 |  1969 | 24 |  3.23 |    3.70 |  23.92
##  1970 |  1970 | 39 |  5.24 |    6.02 |  29.94
##  1971 |  1971 | 40 |  5.38 |    6.17 |  36.11
##  1972 |  1972 | 40 |  5.38 |    6.17 |  42.28
##  1973 |  1973 | 31 |  4.17 |    4.78 |  47.07
##  1974 |  1974 | 36 |  4.84 |    5.56 |  52.62
##  1975 |  1975 | 21 |  2.82 |    3.24 |  55.86
##  1976 |  1976 | 24 |  3.23 |    3.70 |  59.57
##  1977 |  1977 | 30 |  4.03 |    4.63 |  64.20
##  1978 |  1978 | 31 |  4.17 |    4.78 |  68.98
##  1979 |  1979 | 21 |  2.82 |    3.24 |  72.22
##  1980 |  1980 | 39 |  5.24 |    6.02 |  78.24
##  1981 |  1981 | 35 |  4.70 |    5.40 |  83.64
##  1982 |  1982 | 31 |  4.17 |    4.78 |  88.43
##  1983 |  1983 | 14 |  1.88 |    2.16 |  90.59
##  1984 |  1984 | 15 |  2.02 |    2.31 |  92.90
##  1985 |  1985 | 14 |  1.88 |    2.16 |  95.06
##  1986 |  1986 | 12 |  1.61 |    1.85 |  96.91
##  1987 |  1987 |  6 |  0.81 |    0.93 |  97.84
##  1988 |  1988 |  5 |  0.67 |    0.77 |  98.61
##  1989 |  1989 |  4 |  0.54 |    0.62 |  99.23
##  1990 |  1990 |  1 |  0.13 |    0.15 |  99.38
##  1991 |  1991 |  2 |  0.27 |    0.31 |  99.69
##  1992 |  1992 |  2 |  0.27 |    0.31 | 100.00
##  9999 |    Nr |  0 |  0.00 |    0.00 | 100.00
##  <NA> |  <NA> | 96 | 12.90 |    <NA> |   <NA>
```

##### Otros ajustes
Se crea variable edad númerica.


```r
# Crear variable edad
ap_proc$edad_ap <- 2019 - ap_proc$nacimiento

# ---- Descriptivo nueva variable ----
summary(ap_proc$edad_ap)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   27.00   39.00   45.00   44.82   49.00   86.00      96
```

```r
# ---- Etiquetado variables nuevas ----
ap_proc$edad_ap <- set_label(x = ap_proc$edad_ap, label = "Edad apoderado")
```


#### Demasiada desigualdad (N1)

##### Descriptivos

```r
frq(ap_proc$dem_desig)
```

```
## 
## P10A ¿Cuán de acuerdo o en desacuerdo está usted con las siguientes afirmaciones (x) <numeric>
## # total N=744  valid N=744  mean=3.76  sd=1.07
## 
## Value |             Label |   N | Raw % | Valid % | Cum. %
## ----------------------------------------------------------
##     1 | Muy en desacuerdo |  11 |  1.48 |    1.48 |   1.48
##     2 |     En desacuerdo |  26 |  3.49 |    3.49 |   4.97
##     3 |        De acuerdo | 191 | 25.67 |   25.67 |  30.65
##     4 |    Muy de acuerdo | 496 | 66.67 |   66.67 |  97.31
##     9 |                Nr |  20 |  2.69 |    2.69 | 100.00
##  <NA> |              <NA> |   0 |  0.00 |    <NA> |   <NA>
```

##### Recodificacion

```r
ap_proc$dem_desig <- set_na(ap_proc$dem_desig, na = c(9), drop.levels = TRUE, as.tag = FALSE)
```

##### Etiquetado

```r
ap_proc$dem_desig <- set_label(x = ap_proc$dem_desig, label = "Las diferencias económicas en Chile son demasiado grandes")
```

##### Descriptivo post-rec

```r
frq(ap_proc$dem_desig)
```

```
## 
## Las diferencias económicas en Chile son demasiado grandes (x) <numeric>
## # total N=744  valid N=724  mean=3.62  sd=0.63
## 
## Value |             Label |   N | Raw % | Valid % | Cum. %
## ----------------------------------------------------------
##     1 | Muy en desacuerdo |  11 |  1.48 |    1.52 |   1.52
##     2 |     En desacuerdo |  26 |  3.49 |    3.59 |   5.11
##     3 |        De acuerdo | 191 | 25.67 |   26.38 |  31.49
##     4 |    Muy de acuerdo | 496 | 66.67 |   68.51 | 100.00
##  <NA> |              <NA> |  20 |  2.69 |    <NA> |   <NA>
```

##### Otros ajustes

```r
# Transformacion a factor
ap_proc$dem_desig_factor <- factor(ap_proc$dem_desig, levels = c(1,2,3,4), labels = c("Muy en desacuerdo", "En desacuerdo", "De acuerdo", "Muy de acuerdo"))

# Etiquetado
ap_proc$dem_desig_factor <- set_label(x = ap_proc$dem_desig_factor, label = "Las diferencias económicas en Chile son demasiado grandes")
```


#### Género (Control N1)
##### Descriptivo

```r
# Genero apoderados
frq(ap_proc$genero)
```

```
## 
## P39 ¿Cuál es su género? (x) <numeric>
## # total N=744  valid N=744  mean=1.81  sd=0.39
## 
## Value |  Label |   N | Raw % | Valid % | Cum. %
## -----------------------------------------------
##     1 | Hombre | 141 | 18.95 |   18.95 |  18.95
##     2 |  Mujer | 602 | 80.91 |   80.91 |  99.87
##     3 |   Otro |   1 |  0.13 |    0.13 | 100.00
##     9 |     Nr |   0 |  0.00 |    0.00 | 100.00
##  <NA> |   <NA> |   0 |  0.00 |    <NA> |   <NA>
```

##### Recodificación
Se transforma el valor 9 (Nr).


```r
# Genero apoderados
ap_proc$genero <- set_na(ap_proc$genero, na = c(9), drop.levels = TRUE, as.tag = FALSE)
```

##### Etiquetado

```r
# Genero apoderados
ap_proc$genero <- set_label(x = ap_proc$genero, label = "Genero apoderados")
```

##### Descriptivo post-rec

```r
# Genero apoderados
frq(ap_proc$genero)
```

```
## 
## Genero apoderados (x) <numeric>
## # total N=744  valid N=744  mean=1.81  sd=0.39
## 
## Value |  Label |   N | Raw % | Valid % | Cum. %
## -----------------------------------------------
##     1 | Hombre | 141 | 18.95 |   18.95 |  18.95
##     2 |  Mujer | 602 | 80.91 |   80.91 |  99.87
##     3 |   Otro |   1 |  0.13 |    0.13 | 100.00
##  <NA> |   <NA> |   0 |  0.00 |    <NA> |   <NA>
```

##### Otros ajustes
Crear variable factor


```r
# Transformación a factor
ap_proc$genero_factor <- factor(ap_proc$genero, levels = c(1,2,3), labels = c("Hombre", "Mujer", "Otro"))

# Etiquetar
ap_proc$genero_factor <- set_label(x = ap_proc$genero_factor, label = "Genero apoderados factor")

# Crear variable Porcentaje de apoderadas mujeres en la escuela (N2)

porc_muj_esc <- ap_proc %>%group_by(rbd)%>%summarise(porc_muj_esc = length(which(genero == 2))/(length(which(genero == 1)) + length(which(genero == 2)) + length(which(genero == 3))))

ap_proc <- left_join(ap_proc, porc_muj_esc, by = "rbd")
ap_proc$porc_muj_esc <- set_label(ap_proc$porc_muj_esc,label = "Porcentaje de apoderadas mujeres en la escuela")
```


#### Posición política (Control N1)
##### Descriptivo

```r
# Posición política apoderados
frq(ap_proc$pos_pol)
```

```
## 
## P23 Tradicionalmente la gente define las posiciones políticas como más cercanas  (x) <numeric>
## # total N=744  valid N=744  mean=5.92  sd=2.22
## 
## Value |            Label |   N | Raw % | Valid % | Cum. %
## ---------------------------------------------------------
##     1 |          Derecha |  58 |  7.80 |    7.80 |   7.80
##     2 |   Centro derecha |  50 |  6.72 |    6.72 |  14.52
##     3 |           Centro |  21 |  2.82 |    2.82 |  17.34
##     4 | Centro Izquierda |  42 |  5.65 |    5.65 |  22.98
##     5 |        Izquierda |  50 |  6.72 |    6.72 |  29.70
##     6 |    Independiente |  49 |  6.59 |    6.59 |  36.29
##     7 |          Ninguna | 356 | 47.85 |   47.85 |  84.14
##     8 |            No sé |  80 | 10.75 |   10.75 |  94.89
##     9 |               Nr |  38 |  5.11 |    5.11 | 100.00
##  <NA> |             <NA> |   0 |  0.00 |    <NA> |   <NA>
```

```r
summary(ap_proc$pos_pol)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   5.000   7.000   5.923   7.000   9.000
```

##### Recodificación
Se recodifican a "Ns/Nr" los valores 8 (No sabe) y 9 (Nr).


```r
# Posición política apoderados
ap_proc$pos_pol[ap_proc$pos_pol == 8 | ap_proc$pos_pol == 9] <- 99
ap_proc$pos_pol <- set_labels(ap_proc$pos_pol,
            labels=c("Right" = 1,
                     "Center Right" = 2,
                     "Center" = 3,
                     "Center Left" = 4,
                     "Left" = 5,
                     "Independent" = 6,
                     "None" = 7,
                     "Na/Nr"=99)) #Etiquetado categorías
```

##### Etiquetado

```r
# Posición política apoderados
ap_proc$pos_pol <- set_label(x = ap_proc$pos_pol, label = "Political position parents")
```

##### Descriptivo post-rec

```r
# Posición política apoderados
frq(ap_proc$pos_pol)
```

```
## 
## Political position parents (x) <numeric>
## # total N=744  valid N=744  mean=20.31  sd=34.25
## 
## Value |        Label |   N | Raw % | Valid % | Cum. %
## -----------------------------------------------------
##     1 |        Right |  58 |  7.80 |    7.80 |   7.80
##     2 | Center Right |  50 |  6.72 |    6.72 |  14.52
##     3 |       Center |  21 |  2.82 |    2.82 |  17.34
##     4 |  Center Left |  42 |  5.65 |    5.65 |  22.98
##     5 |         Left |  50 |  6.72 |    6.72 |  29.70
##     6 |  Independent |  49 |  6.59 |    6.59 |  36.29
##     7 |         None | 356 | 47.85 |   47.85 |  84.14
##    99 |        Na/Nr | 118 | 15.86 |   15.86 | 100.00
##  <NA> |         <NA> |   0 |  0.00 |    <NA> |   <NA>
```

##### Otros ajustes
Crear variable factor


```r
# Transformación a factor
ap_proc$pos_pol_factor <- factor(ap_proc$pos_pol, levels = c(1,2,3,4,5,6,7,99), labels = c("Derecha", "Centro Derecha", "Centro", "Centro Izquierda", "Izquierda", "Independiente", "Ninguna", "Ns/Nr"))

# Etiquetar
ap_proc$pos_pol_factor <- set_label(x = ap_proc$pos_pol_factor, label = "Political position parents")
```


#### Cantidad de libros en el hogar (Control N1)
##### Descriptivo

```r
# Cuestionario apoderados
frq(ap_proc$libros_hogar)
```

```
## 
## P47 Aproximadamente ¿Cuántos libros hay en su hogar? (x) <numeric>
## # total N=744  valid N=744  mean=2.87  sd=1.77
## 
## Value |                  Label |   N | Raw % | Valid % | Cum. %
## ---------------------------------------------------------------
##     1 |    Entre 0 y 10 libros | 139 | 18.68 |   18.68 |  18.68
##     2 |   Entre 11 y 25 libros | 204 | 27.42 |   27.42 |  46.10
##     3 |  Entre 26 y 100 libros | 247 | 33.20 |   33.20 |  79.30
##     4 | Entre 101 y 200 libros |  76 | 10.22 |   10.22 |  89.52
##     5 | Entre 201 y 500 libros |  33 |  4.44 |    4.44 |  93.95
##     6 |      Más de 500 libros |   9 |  1.21 |    1.21 |  95.16
##     9 |                     Nr |  36 |  4.84 |    4.84 | 100.00
##  <NA> |                   <NA> |   0 |  0.00 |    <NA> |   <NA>
```

##### Recodificación
Se transforma el valor 9 (Nr) a NA.


```r
# Cuestionarios apoderados
ap_proc$libros_hogar <- set_na(ap_proc$libros_hogar, na = c(9), drop.levels = TRUE, as.tag = FALSE)
```

##### Etiquetado

```r
# Cuestionario apoderados
ap_proc$libros_hogar <- set_label(x = ap_proc$libros_hogar, label = "Books in household (parents)")
```

##### Descriptivo post-rec

```r
# Cuestionario apoderados
frq(ap_proc$libros_hogar)
```

```
## 
## Books in household (parents) (x) <numeric>
## # total N=744  valid N=708  mean=2.56  sd=1.13
## 
## Value |                  Label |   N | Raw % | Valid % | Cum. %
## ---------------------------------------------------------------
##     1 |    Entre 0 y 10 libros | 139 | 18.68 |   19.63 |  19.63
##     2 |   Entre 11 y 25 libros | 204 | 27.42 |   28.81 |  48.45
##     3 |  Entre 26 y 100 libros | 247 | 33.20 |   34.89 |  83.33
##     4 | Entre 101 y 200 libros |  76 | 10.22 |   10.73 |  94.07
##     5 | Entre 201 y 500 libros |  33 |  4.44 |    4.66 |  98.73
##     6 |      Más de 500 libros |   9 |  1.21 |    1.27 | 100.00
##  <NA> |                   <NA> |  36 |  4.84 |    <NA> |   <NA>
```

##### Otros ajustes 
Crear variable factor


```r
# Transformación a factor
ap_proc$libros_hogar_factor <- factor(ap_proc$libros_hogar, levels = c(1,2,3,4,5,6), labels = c("Between 0 and 10 books", "Between 11 and 25 books", "Between 26 and 100 book", "Between 101 and 200 books", "Between 201 and 500 books", "More than 500 books"))

# Etiquetar
ap_proc$libros_hogar_factor <- set_label(x = ap_proc$libros_hogar_factor, label = "Books in household (parents)")
```


#### Region (Control N1)

##### Descriptivo

```r
# Cuestionario apoderados
frq(ap_proc$region)
```

```
## 
## x <numeric>
## # total N=744  valid N=744  mean=9.40  sd=4.25
## 
## Value |                 Label |   N | Raw % | Valid % | Cum. %
## --------------------------------------------------------------
##     2 | Región de Antofagasta | 127 | 17.07 |   17.07 |  17.07
##     7 |      Región del Maule | 214 | 28.76 |   28.76 |  45.83
##    13 |  Región Metropolitana | 403 | 54.17 |   54.17 | 100.00
##  <NA> |                  <NA> |   0 |  0.00 |    <NA> |   <NA>
```

##### Recodificación
No aplica.

##### Etiquetado

```r
# Cuestionario apoderados
ap_proc$region <- set_label(x = ap_proc$region, label = "Region")
```

##### Descriptivo post-rec
No aplica.

##### Otros ajustes
Crear variable factor


```r
# Transformación a factor
ap_proc$region_factor <- factor(ap_proc$region, levels = c(2,7,13), labels = c("Región de Antofagasta", "Región del Maule", "Región Metropolitana"))

# Etiquetar
ap_proc$region_factor <- set_label( x = ap_proc$region_factor, label = "Region apoderados factor")
```

#### Recompensa (Control N1)
##### Descriptivo 

```r
# Cuestionario apoderados
frq(ap_proc$recompensa)
```

```
## 
## P10C ¿Cuán de acuerdo o en desacuerdo está usted con las siguientes afirmaciones (x) <numeric>
## # total N=744  valid N=744  mean=3.32  sd=1.70
## 
## Value |             Label |   N | Raw % | Valid % | Cum. %
## ----------------------------------------------------------
##     1 | Muy en desacuerdo |  35 |  4.70 |    4.70 |   4.70
##     2 |     En desacuerdo | 141 | 18.95 |   18.95 |  23.66
##     3 |        De acuerdo | 372 | 50.00 |   50.00 |  73.66
##     4 |    Muy de acuerdo | 146 | 19.62 |   19.62 |  93.28
##     9 |                Nr |  50 |  6.72 |    6.72 | 100.00
##  <NA> |              <NA> |   0 |  0.00 |    <NA> |   <NA>
```

##### Recodificacion.
Se transforman los vales 9 (Nr) a NA.


```r
# Cuestionario apoderados
ap_proc$recompensa <- set_na(ap_proc$recompensa, na = c(9), drop.levels = TRUE, as.tag = FALSE)
```

##### Etiquetado

```r
# Cuestionario apoderados
ap_proc$recompensa <- set_label(x = ap_proc$recompensa, label = "Esfuerzo es recompensado en escuela (apoderados)")
```

##### Descriptivo post-rec

```r
# Cuestionario apoderados
frq(ap_proc$recompensa)
```

```
## 
## Esfuerzo es recompensado en escuela (apoderados) (x) <numeric>
## # total N=744  valid N=694  mean=2.91  sd=0.78
## 
## Value |             Label |   N | Raw % | Valid % | Cum. %
## ----------------------------------------------------------
##     1 | Muy en desacuerdo |  35 |  4.70 |    5.04 |   5.04
##     2 |     En desacuerdo | 141 | 18.95 |   20.32 |  25.36
##     3 |        De acuerdo | 372 | 50.00 |   53.60 |  78.96
##     4 |    Muy de acuerdo | 146 | 19.62 |   21.04 | 100.00
##  <NA> |              <NA> |  50 |  6.72 |    <NA> |   <NA>
```

##### Otros ajustes
Crear variable factor

```r
# Transformación a factor
ap_proc$recompensa_factor <- factor(ap_proc$recompensa, levels = c(1,2,3,4), labels = c("Muy en desacuerdo", "En desacuerdo", "De acuerdo", "Muy de acuerdo"))

# Etiquetar
ap_proc$recompensa_factor <- set_label(x = ap_proc$recompensa_factor, label = "Esfuerzo es recompensado en escuela (apoderados)")
```


#### Cantidad de personas en el hogar (Control N1)
##### Descriptivo

```r
frq(ap_proc$personas_hogar)
```

```
## 
## P41 Considerando al estudiante ¿Cuántas personas viven en su hogar? (x) <numeric>
## # total N=744  valid N=744  mean=73.95  sd=253.75
## 
## Value | Label |   N | Raw % | Valid % | Cum. %
## ----------------------------------------------
##     1 |     1 |   1 |  0.13 |    0.13 |   0.13
##     2 |     2 |  25 |  3.36 |    3.36 |   3.49
##     3 |     3 | 139 | 18.68 |   18.68 |  22.18
##     4 |     4 | 253 | 34.01 |   34.01 |  56.18
##     5 |     5 | 165 | 22.18 |   22.18 |  78.36
##     6 |     6 |  60 |  8.06 |    8.06 |  86.42
##     7 |     7 |  25 |  3.36 |    3.36 |  89.78
##     8 |     8 |  10 |  1.34 |    1.34 |  91.13
##     9 |     9 |   5 |  0.67 |    0.67 |  91.80
##    10 |    10 |   5 |  0.67 |    0.67 |  92.47
##    12 |    12 |   2 |  0.27 |    0.27 |  92.74
##    15 |    15 |   2 |  0.27 |    0.27 |  93.01
##   999 |    Nr |  52 |  6.99 |    6.99 | 100.00
##  <NA> |  <NA> |   0 |  0.00 |    <NA> |   <NA>
```

##### Recodificacion
Se transforma a NA el valor 999, correspondiente a "Nr"


```r
ap_proc$personas_hogar[ap_proc$personas_hogar == 999] <- NA
```

##### Etiquetado

```r
ap_proc$personas_hogar <- set_label(x = ap_proc$personas_hogar, label = "Cantidad de personas en el hogar")
```

##### Descriptivo post-rec

```r
frq(ap_proc$personas_hogar)
```

```
## 
## Cantidad de personas en el hogar (x) <numeric>
## # total N=744  valid N=692  mean=4.43  sd=1.51
## 
## Value | Label |   N | Raw % | Valid % | Cum. %
## ----------------------------------------------
##     1 |     1 |   1 |  0.13 |    0.14 |   0.14
##     2 |     2 |  25 |  3.36 |    3.61 |   3.76
##     3 |     3 | 139 | 18.68 |   20.09 |  23.84
##     4 |     4 | 253 | 34.01 |   36.56 |  60.40
##     5 |     5 | 165 | 22.18 |   23.84 |  84.25
##     6 |     6 |  60 |  8.06 |    8.67 |  92.92
##     7 |     7 |  25 |  3.36 |    3.61 |  96.53
##     8 |     8 |  10 |  1.34 |    1.45 |  97.98
##     9 |     9 |   5 |  0.67 |    0.72 |  98.70
##    10 |    10 |   5 |  0.67 |    0.72 |  99.42
##    12 |    12 |   2 |  0.27 |    0.29 |  99.71
##    15 |    15 |   2 |  0.27 |    0.29 | 100.00
##   999 |    Nr |   0 |  0.00 |    0.00 | 100.00
##  <NA> |  <NA> |  52 |  6.99 |    <NA> |   <NA>
```

#### Otros ajustes
No aplica.

#### NSE: Ingresos por escuela (Var. Indep. N2)
##### Otros ajustes
Se construyen dos variables para el ingreso a nivel escuela. La primera a partir de la agregación del ingreso del hogar. La segunda a partir de la agregación del ingreso per capita.


```r
# Ingresos a nivel escuela
 ap_proc <- ap_proc %>%
     group_by(rbd) %>%
     mutate(ingresos_esc = mean(ingresos, na.rm = T))

# Ingresos per capita a nivel escuela
 ap_proc <- ap_proc %>%
     group_by(rbd) %>%
     mutate(ingresos_pc_esc = mean(ingresos_pc, na.rm = T))
 
#---- Descriptivos variables nuevas----
# Ingresos a nivel escuela
summary(ap_proc$ingresos_esc)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  187500  418776  649289  882508 1062661 3000000       7
```

```r
# Ingresos per capita a nivel escuela
summary(ap_proc$ingresos_pc_esc) 
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   43368   95122  166390  207756  272385  947400       7
```

```r
#---- Etiquetado variables nuevas----
# Ingresos a nivel escuela
ap_proc$ingresos_esc <- set_label(x = ap_proc$ingresos_esc, label = "Promedio ingresos por escuela")

# Ingresos per capita a nivel escuela
ap_proc$ingresos_pc_esc <- set_label(x = ap_proc$ingresos_pc_esc, label = "Ingreso per capita por escuela")
```

#### NSE: Educacion del apoderado por escuela (Var. Indep. N2)
##### Otros ajustes

```r
# Construcción educación terciara apoderado por escuela

ap_proc$univ<- ifelse(ap_proc$educ==4,1,0)
 ap_proc <- ap_proc %>%
     group_by(rbd) %>%
     mutate(univ_esc = mean(univ, na.rm = T))

# ---- Descriptivos nuevas variables ----
frq(ap_proc$univ)
```

```
## 
## x <numeric>
## # total N=744  valid N=707  mean=0.19  sd=0.39
## 
## Value |   N | Raw % | Valid % | Cum. %
## --------------------------------------
##     0 | 574 | 77.15 |   81.19 |  81.19
##     1 | 133 | 17.88 |   18.81 | 100.00
##  <NA> |  37 |  4.97 |    <NA> |   <NA>
```

```r
summary(ap_proc$univ_esc)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.0000  0.0000  0.1000  0.1885  0.2857  0.9091
```

```r
#---- Etiquetado nuevas variables ----
ap_proc$univ <- set_label(x = ap_proc$univ, label = "Educacion terciaria apoderados") # Etiquetado variable

ap_proc$univ <- set_labels(ap_proc$univ,
            labels=c( "Universitario"=1,
                      "No universitario"=0)) #Etiquetado categorias

ap_proc$univ_esc <- set_label(x = ap_proc$univ_esc, label = "Porcentaje ed. terciaria por escuela")
```


#### Dependencia administrativa escuela (Control N2)
##### Descriptivo

```r
# Cuesitionarios apoderados
frq(ap_proc$dependencia)
```

```
## 
## Depedencia (x) <numeric>
## # total N=744  valid N=744  mean=1.83  sd=0.66
## 
## Value |               Label |   N | Raw % | Valid % | Cum. %
## ------------------------------------------------------------
##     1 |           Municipal | 238 | 31.99 |   31.99 |  31.99
##     2 | Part. Subvencionado | 396 | 53.23 |   53.23 |  85.22
##     3 |       Part. Privado | 110 | 14.78 |   14.78 | 100.00
##  <NA> |                <NA> |   0 |  0.00 |    <NA> |   <NA>
```

```r
summary(ap_proc$dependencia)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   1.000   2.000   1.828   2.000   3.000
```
##### Recodificación
No aplica.

##### Etiquetado

```r
# Etiquetar
ap_proc$dependencia <- set_label(x = ap_proc$dependencia, label = "Dependencia apoderados")
```


##### Descriptivo post-rec
No aplica.

##### Otros ajustes
Crear variable factor

```r
# Transformacion a factor
ap_proc$dependencia_factor <- factor(ap_proc$dependencia, levels = c(1,2,3), labels = c("Municipal", "Part. Subvencionado", "Part. Privado"))

# Etiquetar
ap_proc$dependencia_factor <- set_label(x = ap_proc$dependencia_factor, label = "Dependencia administrativa")
```


#### Heterogeneidad socioeconómica de la escuela (Control N2)
##### Otros ajustes
Se construye variable de heterogeneidad socioeconomica de la escuela a partir del ingreso a nivel individual.

```r
# Construccion heterogeneidad

ap_proc <- ap_proc %>%
     group_by(rbd) %>%
     mutate(heterogen_esc = sd(ingresos, na.rm = T)) 

#---- Descriptivo variables nuevas ----

summary(ap_proc$heterogen_esc)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   38683  209808  445474  571290  806870 2189910       7
```

```r
#---- Etiquetado variables nuevas ----
ap_proc$heterogen_esc <- set_label(x = ap_proc$heterogen_esc, label = "Heterogeneidad de la escuela")
```

## Parte 5: Merge, tabla descriptivo general, ultimos ajustes y guardar bases

Unir bases.

```r
# Join
ap_est <- left_join(x = est_proc,y = ap_proc,by =c("folio"="folio"),suffix=c("_est","_ap"))
```

Ajustes post-merge.
Educación (N1): Combinación datos apoderados y estudiantes

```r
# Guardar respuestas del nivel educacional de los padres. Si no respondió, se incluye la respuesta más alta reportada por los estudiantes.
ap_est$educ_ap <- ifelse(is.na(ap_est$educ), ap_est$educ_padres, ap_est$educ)
ap_est$educ_ap <- set_label(x = ap_est$educ,label = "Nivel educacional de los apoderados")
```

Crear variable factor para educación

```r
# Transformacion a factor
ap_est$educ_factor_ap <- factor(ap_est$educ_ap, 
                                levels = c(1,2,3,4), 
                                labels = c("Primary or less", "High Highschool", "Vocational", "Universitary degree or more"))
ap_est$educ_factor_ap <- set_label(x = ap_est$educ_factor_ap,label = "Educational level (parents)") # Etiquetar
```

Centrado de variables a la media grupal

```r
# Centrado a la media grupal (CWC) (rbd_est)

## Promedio percepción meritocracia apoderados
ap_est$prom_percmer_ap_cwc <- center(ap_est$prom_percmer_ap, type = "CWC", group = ap_est$rbd_est)
ap_est$prom_percmer_ap_cwc <- set_label(x = ap_est$prom_percmer_ap_cwc, label = "Promedio Percepción meritocratica apoderados (CWC)")

## Sentido justicia indirecto Ln
ap_est$sj_indirect_ln_cwc <- center(ap_est$sj_indirect_ln, type = "CWC", group = ap_est$rbd_est)
ap_est$sj_indirect_ln_cwc <- set_label(x = ap_est$sj_indirect_ln_cwc, label = "Sentido justicia indirecto ln (CWC)")

## Sentido justicia directo
ap_est$sj_direct_cwc <- center(ap_est$sj_direct, type = "CWC", group = ap_est$rbd_est)
ap_est$sj_direct_cwc <- set_label(x = ap_est$sj_direct_cwc, label = "Sentido justicia directo (CWC)")

## Posición Política estudiante
ap_est$pos_pol_est_cwc <- center(ap_est$pos_pol_est, type = "CWC", group = ap_est$rbd_est)
ap_est$pos_pol_est_cwc <- set_label(x = ap_est$pos_pol_est_cwc, "Posición política estudiantes (CWC)")

## Recompensa estudiante
ap_est$recompensa_est_cwc <- center(ap_est$recompensa_est, type = "CWC", group = ap_est$rbd_est)
ap_est$recompensa_est_cwc <- set_label(x = ap_est$recompensa_est_cwc, label = "Esfuerzo es recompensado en escuela (estudiantes - CWC)")

## La opinion es tomada en cuenta por los profesores
ap_est$resp_prof_cwc <- center(ap_est$resp_prof, type = "CWC", group = ap_est$rbd_est)
ap_est$resp_prof_cwc <- set_label(x = ap_est$resp_prof_cwc, label = "Justicia en el trato profesores (CWC)")

# Promedio de notas obtenido
ap_est$prom_obt_cwc <- center(ap_est$prom_obt, type = "CWC", group = ap_est$rbd_est)
ap_est$prom_obt_cwc <- set_label(x = ap_est$prom_obt_cwc, label = "Promedio de notas obtenido (CWC)")

# Promedio de notas merecido
ap_est$prom_mer_cwc <- center(ap_est$prom_mer, type = "CWC", group = ap_est$rbd_est)
ap_est$prom_mer_cwc <- set_label(x = ap_est$prom_mer_cwc, label = "Promedio de notas obtenido CWC")

## Posición Política apoderado
ap_est$pos_pol_ap_cwc <- center(ap_est$pos_pol_ap, type = "CWC", group = ap_est$rbd_est)
ap_est$pos_pol_ap_cwc <- set_label(x = ap_est$pos_pol_ap_cwc, label = "Posición política (apoderados - CWC)" )

## Educación apoderado
ap_est$educ_ap_cwc <- center(ap_est$educ_ap, type = "CWC", group = ap_est$rbd_est)
ap_est$educ_ap_cwc <- set_label(x = ap_est$educ_ap_cwc, label = "Nivel educacional más alto apoderados (CWC)")

## Libros Hogar apoderado
ap_est$libros_hogar_ap_cwc <- center(ap_est$libros_hogar_ap, type = "CWC", group = ap_est$rbd_est)

ap_est$libros_hogar_ap_cwc <- set_label(x = ap_est$libros_hogar_ap_cwc, label = "Libros en el hogar (apoderados - CWC)")

## Recompensa apoderado
ap_est$recompensa_ap_cwc <- center(ap_est$recompensa_ap, type = "CWC", group = ap_est$rbd_est)
ap_est$recompensa_ap_cwc <- set_label(x = ap_est$recompensa_ap_cwc, label = "Esfuerzo es recompensado en escuela (apoderados - CWC)")

## Edad apoderado
ap_est$edad_ap_cwc <- center(ap_est$edad_ap, type = "CWC", group = ap_est$rbd_est)
ap_est$edad_ap_cwc <- set_label(x = ap_est$edad_ap_cwc, label = "Edad apoderados (CWC)")

## Percepción desigualdad apoderados
ap_est$dem_desig_cwc <- center(ap_est$dem_desig, type = "CWC", group = ap_est$rbd_est)
ap_est$dem_desig_cwc <- set_label(x = ap_est$dem_desig_cwc, label = "Percepción desigualdad (CWC)")
```




### Dummy percepción meritocracia estudiantes


```r
#Importancia trabajo duro (estudiante): 1 = 0; 2,3,4 = 1
ap_est$perc_tduro_bi_est <- car::recode(ap_est$perc_trabajo_duro_est,recodes = "1=0;2:4=1",as.factor = T) 
table(ap_est$perc_tduro_bi_est)
```

```
## 
##    0    1 
##   69 1519
```

```r
ap_est$perc_tduro_bi_est <- factor(ap_est$perc_tduro_bi_est,labels = c("Nada importante","Es importante"))
table(ap_est$perc_tduro_bi_est)
```

```
## 
## Nada importante   Es importante 
##              69            1519
```

```r
ap_est$perc_tduro_bi_est <- set_label(ap_est$perc_tduro_bi_est, label = "Importancia del trabajo duro (estudiante)")
frq(ap_est$perc_tduro_bi_est)
```

```
## 
## Importancia del trabajo duro (estudiante) (x) <categorical>
## # total N=1635  valid N=1588  mean=1.96  sd=0.20
## 
## Value           |    N | Raw % | Valid % | Cum. %
## -------------------------------------------------
## Nada importante |   69 |  4.22 |    4.35 |   4.35
## Es importante   | 1519 | 92.91 |   95.65 | 100.00
## <NA>            |   47 |  2.87 |    <NA> |   <NA>
```

```r
# Importancia trabajo duro (estudiante): 1,2 = 0; 3,4 = 1 
ap_est$perc_tduro_bi2_est <- car::recode(ap_est$perc_trabajo_duro_est,recodes = "1:2=0;3:4=1",as.factor = T) 
table(ap_est$perc_tduro_bi2_est)
```

```
## 
##    0    1 
##  263 1325
```

```r
ap_est$perc_tduro_bi2_est <- factor(ap_est$perc_tduro_bi2_est,labels = c("Nada importante","Es importante"))
table(ap_est$perc_tduro_bi2_est)
```

```
## 
## Nada importante   Es importante 
##             263            1325
```

```r
ap_est$perc_tduro_bi2_est <- set_label(ap_est$perc_tduro_bi2_est, label = "Importancia del trabajo duro (estudiante)")
frq(ap_est$perc_tduro_bi2_est)
```

```
## 
## Importancia del trabajo duro (estudiante) (x) <categorical>
## # total N=1635  valid N=1588  mean=1.83  sd=0.37
## 
## Value           |    N | Raw % | Valid % | Cum. %
## -------------------------------------------------
## Nada importante |  263 | 16.09 |   16.56 |  16.56
## Es importante   | 1325 | 81.04 |   83.44 | 100.00
## <NA>            |   47 |  2.87 |    <NA> |   <NA>
```

```r
# Importancia trabajo duro (estudiante): 1,2,3 = 0; 4 = 1 
ap_est$perc_tduro_bi3_est <- car::recode(ap_est$perc_trabajo_duro_est,recodes = "1:3=0;4=1",as.factor = T) 
table(ap_est$perc_tduro_bi3_est)
```

```
## 
##   0   1 
## 676 912
```

```r
ap_est$perc_tduro_bi3_est <- factor(ap_est$perc_tduro_bi3_est,labels = c("Nada importante","Es importante"))
table(ap_est$perc_tduro_bi3_est)
```

```
## 
## Nada importante   Es importante 
##             676             912
```

```r
ap_est$perc_tduro_bi3_est <- set_label(ap_est$perc_tduro_bi3_est, label = "Importancia del trabajo duro (estudiante)")
frq(ap_est$perc_tduro_bi3_est)
```

```
## 
## Importancia del trabajo duro (estudiante) (x) <categorical>
## # total N=1635  valid N=1588  mean=1.57  sd=0.49
## 
## Value           |   N | Raw % | Valid % | Cum. %
## ------------------------------------------------
## Nada importante | 676 | 41.35 |   42.57 |  42.57
## Es importante   | 912 | 55.78 |   57.43 | 100.00
## <NA>            |  47 |  2.87 |    <NA> |   <NA>
```

```r
ap_est$perc_esfuerzo_bi_est <- car::recode(ap_est$perc_esfuerzo_est,recodes = "1:2=0;3:4=1",as.factor = T) 
table(ap_est$perc_esfuerzo_bi_est)
```

```
## 
##    0    1 
##  286 1282
```

```r
ap_est$perc_esfuerzo_bi_est <- factor(ap_est$perc_esfuerzo_bi_est,labels = c("En desacuerdo","De acuerdo"))
table(ap_est$perc_tduro_bi_est)
```

```
## 
## Nada importante   Es importante 
##              69            1519
```

```r
ap_est$perc_esfuerzo_bi_est <- set_label(ap_est$perc_esfuerzo_bi_est, label = "Esfuerzo es recompensado (estudiante)")
frq(ap_est$perc_esfuerzo_bi_est)
```

```
## 
## Esfuerzo es recompensado (estudiante) (x) <categorical>
## # total N=1635  valid N=1568  mean=1.82  sd=0.39
## 
## Value         |    N | Raw % | Valid % | Cum. %
## -----------------------------------------------
## En desacuerdo |  286 | 17.49 |   18.24 |  18.24
## De acuerdo    | 1282 | 78.41 |   81.76 | 100.00
## <NA>          |   67 |  4.10 |    <NA> |   <NA>
```

### Dummy percepción meritocracia padres


```r
#Importancia trabajo duro (apoderado): 1 = 0; 2,3,4 = 1
ap_est$perc_tduro_bi_ap <- car::recode(ap_est$perc_trabajo_duro_ap,recodes = "1=0;2:4=1",as.factor = T) 
table(ap_est$perc_tduro_bi_ap)
```

```
## 
##   0   1 
##  44 643
```

```r
ap_est$perc_tduro_bi_ap <- factor(ap_est$perc_tduro_bi_ap,labels = c("Nada importante","Es importante"))
table(ap_est$perc_tduro_bi_ap)
```

```
## 
## Nada importante   Es importante 
##              44             643
```

```r
ap_est$perc_tduro_bi_ap <- set_label(ap_est$perc_tduro_bi_ap, label = "Importancia del trabajo duro (apoderado)")
frq(ap_est$perc_tduro_bi_ap)
```

```
## 
## Importancia del trabajo duro (apoderado) (x) <categorical>
## # total N=1635  valid N=687  mean=1.94  sd=0.25
## 
## Value           |   N | Raw % | Valid % | Cum. %
## ------------------------------------------------
## Nada importante |  44 |  2.69 |    6.40 |   6.40
## Es importante   | 643 | 39.33 |   93.60 | 100.00
## <NA>            | 948 | 57.98 |    <NA> |   <NA>
```

```r
# Importancia trabajo duro (apoderado): 1,2 = 0; 3,4 = 1 
ap_est$perc_tduro_bi2_ap <- car::recode(ap_est$perc_trabajo_duro_ap,recodes = "1:2=0;3:4=1",as.factor = T) 
table(ap_est$perc_tduro_bi2_ap)
```

```
## 
##   0   1 
## 129 558
```

```r
ap_est$perc_tduro_bi2_ap <- factor(ap_est$perc_tduro_bi2_ap,labels = c("Nada importante","Es importante"))
table(ap_est$perc_tduro_bi2_ap)
```

```
## 
## Nada importante   Es importante 
##             129             558
```

```r
ap_est$perc_tduro_bi2_ap <- set_label(ap_est$perc_tduro_bi2_ap, label = "Importancia del trabajo duro (apoderado)")
frq(ap_est$perc_tduro_bi2_ap)
```

```
## 
## Importancia del trabajo duro (apoderado) (x) <categorical>
## # total N=1635  valid N=687  mean=1.81  sd=0.39
## 
## Value           |   N | Raw % | Valid % | Cum. %
## ------------------------------------------------
## Nada importante | 129 |  7.89 |   18.78 |  18.78
## Es importante   | 558 | 34.13 |   81.22 | 100.00
## <NA>            | 948 | 57.98 |    <NA> |   <NA>
```

```r
# Importancia trabajo duro (apoderado): 1,2,3 = 0; 4 = 1 
ap_est$perc_tduro_bi3_ap <- car::recode(ap_est$perc_trabajo_duro_ap,recodes = "1:3=0;4=1",as.factor = T) 
table(ap_est$perc_tduro_bi3_ap)
```

```
## 
##   0   1 
## 345 342
```

```r
ap_est$perc_tduro_bi3_ap <- factor(ap_est$perc_tduro_bi3_ap,labels = c("Nada importante","Es importante"))
table(ap_est$perc_tduro_bi3_ap)
```

```
## 
## Nada importante   Es importante 
##             345             342
```

```r
ap_est$perc_tduro_bi3_ap <- set_label(ap_est$perc_tduro_bi3_ap, label = "Importancia del trabajo duro (apoderado)")
frq(ap_est$perc_tduro_bi3_ap)
```

```
## 
## Importancia del trabajo duro (apoderado) (x) <categorical>
## # total N=1635  valid N=687  mean=1.50  sd=0.50
## 
## Value           |   N | Raw % | Valid % | Cum. %
## ------------------------------------------------
## Nada importante | 345 | 21.10 |   50.22 |  50.22
## Es importante   | 342 | 20.92 |   49.78 | 100.00
## <NA>            | 948 | 57.98 |    <NA> |   <NA>
```

```r
ap_est$perc_esfuerzo_bi_ap <- car::recode(ap_est$perc_esfuerzo_ap,recodes = "1:2=0;3:4=1",as.factor = T) 
table(ap_est$perc_esfuerzo_bi_ap)
```

```
## 
##   0   1 
## 123 559
```

```r
ap_est$perc_esfuerzo_bi_ap <- factor(ap_est$perc_esfuerzo_bi_ap,labels = c("En desacuerdo","De acuerdo"))
table(ap_est$perc_tduro_bi_ap)
```

```
## 
## Nada importante   Es importante 
##              44             643
```

```r
ap_est$perc_esfuerzo_bi_ap <- set_label(ap_est$perc_esfuerzo_bi_ap, label = "Esfuerzo es recompensado (apoderado)")
frq(ap_est$perc_esfuerzo_bi_ap)
```

```
## 
## Esfuerzo es recompensado (apoderado) (x) <categorical>
## # total N=1635  valid N=682  mean=1.82  sd=0.38
## 
## Value         |   N | Raw % | Valid % | Cum. %
## ----------------------------------------------
## En desacuerdo | 123 |  7.52 |   18.04 |  18.04
## De acuerdo    | 559 | 34.19 |   81.96 | 100.00
## <NA>          | 953 | 58.29 |    <NA> |   <NA>
```

### Ingreso imputado


```r
ap_est$ingresos_pc.im <- ifelse(test =is.na(ap_est$ingresos_pc),
                                yes = ap_est$ingresos_pc_esc,
                                no=ap_est$ingresos_pc) 
View(ap_est[,c("rbd_est","rbd_ap","ingresos_pc","ingresos_pc_esc","quintiles_ingresos_pc_factor","ingresos_pc.im")])

ap_est$ingresos_pc.im <- set_label(ap_est$ingresos_pc.im,label = "Ingreso per cap (imputado media escuela)")


ap_est$quintiles_ingresos_pc.im <- ntile(ap_est$ingresos_pc.im,5)
# ap_est$quintiles_ingresos_pc.im[is.na(ap_est$quintiles_ingresos_pc.im)] <- 99
# 
# ap_est$quintiles_ingresos_pc.im<- factor(ap_est$quintiles_ingresos_pc.im,
#                                          levels = c(1,2,3,4,5,99),
#                                          labels = c("Quintil 1", "Quintil 2", 
#                                                     "Quintil 3", "Quintil 4", 
#                                                     "Quintil 5", "No sabe/No responde"))

ap_est$quintiles_ingresos_pc.im <- set_label(ap_est$quintiles_ingresos_pc.im, "Quintiles de ingreso per capita del hogar")
sjmisc::frq(ap_est$quintiles_ingresos_pc.im)
```

```
## 
## Quintiles de ingreso per capita del hogar (x) <integer>
## # total N=1635  valid N=710  mean=3.00  sd=1.42
## 
## Value |   N | Raw % | Valid % | Cum. %
## --------------------------------------
##     1 | 142 |  8.69 |      20 |     20
##     2 | 142 |  8.69 |      20 |     40
##     3 | 142 |  8.69 |      20 |     60
##     4 | 142 |  8.69 |      20 |     80
##     5 | 142 |  8.69 |      20 |    100
##  <NA> | 925 | 56.57 |    <NA> |   <NA>
```

```r
sjmisc::frq(ap_est$quintiles_ingresos_pc_factor)
```

```
## 
## Household income  (x) <categorical>
## # total N=1635  valid N=716  mean=3.58  sd=1.74
## 
## Value                  |   N | Raw % | Valid % | Cum. %
## -------------------------------------------------------
## Quintile 1             | 114 |  6.97 |   15.92 |  15.92
## Quintile 2             | 117 |  7.16 |   16.34 |  32.26
## Quintile 3             | 117 |  7.16 |   16.34 |  48.60
## Quintile 4             | 117 |  7.16 |   16.34 |  64.94
## Quintile 5             | 111 |  6.79 |   15.50 |  80.45
## Don't know/No response | 140 |  8.56 |   19.55 | 100.00
## <NA>                   | 919 | 56.21 |    <NA> |   <NA>
```

# Datos administrativos

* Se agregan datos administrativos para cada establecimiento usando RBD
    
    - Grupo socioeconómico (GSE) (cinco grupos)


```r
idps <- 
  data.table::fread(here::here("input/data/original/idps_2m2018.csv"))
data_admin <- 
  idps %>% 
  select(nse_grupo=cod_grupo,
         RBD)
# str(data_admin)
# str(as.numeric(est_proc$rbd))
data_admin <- 
  data_admin %>% 
  filter(RBD %in% as.numeric(ap_est$rbd_est)) %>% 
  data.frame()
ap_est$RBD_delete <-
  as.integer(ap_est$rbd_est)
# class(est_proc$RBD_delete)
# class(data_admin$RBD)
ap_est <- 
  dplyr::full_join(x = ap_est,
                   y = data_admin,
                   by = c("RBD_delete" = "RBD")) %>% 
  select(-RBD_delete)

ap_est$nse_grupo <- 
  sjlabelled::set_labels(ap_est$nse_grupo,
                         labels = c("Bajo",
                                    "Medio bajo",
                                    "Medio",
                                    "Medio alto",
                                    "Alto"))
sjlabelled::set_label(ap_est$nse_grupo) <- "Código de grupo socioeconómico (2018)"
sjmisc::frq(ap_est$nse_grupo)
```

```
## 
## Código de grupo socioeconómico (2018) (x) <integer>
## # total N=1635  valid N=1635  mean=2.78  sd=1.23
## 
## Value |      Label |   N | Raw % | Valid % | Cum. %
## ---------------------------------------------------
##     1 |       Bajo | 223 | 13.64 |   13.64 |  13.64
##     2 | Medio bajo | 560 | 34.25 |   34.25 |  47.89
##     3 |      Medio | 418 | 25.57 |   25.57 |  73.46
##     4 | Medio alto | 218 | 13.33 |   13.33 |  86.79
##     5 |       Alto | 216 | 13.21 |   13.21 | 100.00
##  <NA> |       <NA> |   0 |  0.00 |    <NA> |   <NA>
```















