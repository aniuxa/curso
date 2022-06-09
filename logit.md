Introducción a los modelos generalizados: logit
================
Ana Escoto
17/06/2022

-   [Previo](#previo)
    -   [Paquetería](#paquetería)
-   [Datos](#datos)
-   [Sub-setting para comparar
    modelos](#sub-setting-para-comparar-modelos)
-   [Introducción](#introducción)
-   [Regresión Logística](#regresión-logística)
    -   [Un solo predictor](#un-solo-predictor)
    -   [Predicción de probabilidades](#predicción-de-probabilidades)
    -   [Coeficientes exponenciados](#coeficientes-exponenciados)
    -   [Agregando una variable](#agregando-una-variable)
    -   [Bondad de Ajuste](#bondad-de-ajuste)
    -   [Tabla de modelos estimados](#tabla-de-modelos-estimados)
-   [Regresión Probit](#regresión-probit)
    -   [Un solo predictor](#un-solo-predictor-1)

# Previo

## Paquetería

``` r
#install.packages("sjPlot", dependencies=T) # solito porque da problmas
library(sjPlot)
```

    ## #refugeeswelcome

``` r
if (!require("pacman")) install.packages("pacman") # instala pacman si se requiere
```

    ## Loading required package: pacman

``` r
pacman::p_load(tidyverse, # sobretodo para dplyr
              haven, #importación
              janitor, #tablas
              sjlabelled, # etiquetas
              DescTools, # Paquete para estimaciones y pruebas
              infer, # tidy way 
              broom,  # Una escobita para limpiar (pero es para arreglar)
              estimatr, car, stargazer, ggpubr, 
              jtools, lm.beta, robustbase, sandwich,
              officer,flextable,huxtable, ggstance, kableExtra,
               ResourceSelection, lmtest, mlogit, nnet) # Nuevos
```

# Datos

Vamos a importar la base para Aguascalientes de la Encuesta Nacional de
Ocupación y Empleo, trimestre I de 2021.

> La Encuesta Nacional de Ocupación y Empleo (ENOE) es hoy día la
> encuesta continua levantada en hogares más grande que se aplica en el
> país. Su puesta en marcha en enero del 2005 marcó el fin de un modelo
> de captación y procesamiento que tuvo vigencia durante 20 años, el
> cual correspondió a la Encuesta Nacional de Empleo Urbano (ENEU)
> seguida por la Encuesta Nacional de Empleo (ENE) en donde aquélla
> quedó integrada. (INEGI, 2007)

Más acá: <https://www.inegi.org.mx/programas/enoe/15ymas/>

``` r
SDEMT122 <- read_dta("datos/SDEMT122.dta") 
```

\[Esta base sólo contiene el estado de Aguascalientes para que sea menos
pesada\]

# Sub-setting para comparar modelos

Vamos a hacer una sub-base de nuestras posibles variables explicativas.
Esto es importante porque sólo podemos comparar modelos con la misma
cantidad de observaciones.

``` r
mydata<- SDEMT122 %>% 
  filter(clase2==1) %>%  # me quedo con la población ocupada
  filter(ing_x_hrs>0) %>% # ingresos válidos
  filter(anios_esc<99) %>% # quito missings anios de escolaridad
  filter(eda>14 & eda<99) %>% #PET
  mutate(sex=as_label(sex)) %>% # Ojo, hicimos este cambio en la práctica pasada
  select(eda, sex, anios_esc, ing_x_hrs, pos_ocu, imssissste, medica5c, ent)  
  
tail(mydata)
```

    ## Warning in knit_print.huxtable(ht): Unrecognized output format "gfm-yaml". Using `to_screen` to print huxtables.
    ## Set options("huxtable.knitr_output_format") manually to "latex", "html", "rtf", "docx", "pptx", "md" or "screen".

     ┌─────────────────────────────────────────────────────────────────────
     │ eda   sex      anios_es   ing_x_hr   pos_ocu   imssisss   medica5c  
     │                       c          s             te                   
     ├─────────────────────────────────────────────────────────────────────
     │  52   Hombre          9       25     1         1          3         
     │  68   Hombre          6       26.8   3         4          1         
     │  26   Hombre          9       23.4   1         4          4         
     │  50   Hombre          6       34     1         1          3         
     │  17   Hombre          9       25     1         4          1         
     │  40   Hombre         12       87.2   1         1          3         
     └─────────────────────────────────────────────────────────────────────

Column names: eda, sex, anios_esc, ing_x\_hrs, pos_ocu, imssissste,
medica5c, ent

7/8 columns shown.

Vamos a volver dicotómica (0,1) nuestra variable \[y de paso repasamos
cómo se recodifica en R\]

``` r
mydata$y_binomial<-mydata$imssissste<4
mydata$y_binomial<-as_numeric(mydata$y_binomial)

mydata %>% 
  tabyl(y_binomial)
```

    ## Warning in knit_print.huxtable(ht): Unrecognized output format "gfm-yaml". Using `to_screen` to print huxtables.
    ## Set options("huxtable.knitr_output_format") manually to "latex", "html", "rtf", "docx", "pptx", "md" or "screen".

                        ┌──────────────────────────────┐
                        │ y_binomial       n   percent │
                        ├──────────────────────────────┤
                        │          0    1379     0.483 │
                        │          1    1479     0.517 │
                        └──────────────────────────────┘

Column names: y_binomial, n, percent

# Introducción

En esta práctica vamos a revisar los elementos básicos para la regresión
logística. El proceso en R para todos los modelos generalizados se
parece mucho. Por tanto, no será difícil que luego puedas utilizar otras
funciones de enlace.

Vamos a hacer una sub-base de nuestras posibles variables explicativas.
Esto es importante porque sólo podemos comparar modelos con la misma
cantidad de observaciones. Intentaremos predecir la participación
económica

# Regresión Logística

![ ln\\frac{p(x=1)}{p(x=0)}=\\beta_o+\\beta_1x +\\epsilon](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%20ln%5Cfrac%7Bp%28x%3D1%29%7D%7Bp%28x%3D0%29%7D%3D%5Cbeta_o%2B%5Cbeta_1x%20%2B%5Cepsilon " ln\frac{p(x=1)}{p(x=0)}=\beta_o+\beta_1x +\epsilon")

## Un solo predictor

``` r
modelo0<-glm(y_binomial ~ anios_esc, family = binomial("logit"), data=mydata, na.action=na.exclude)
summary(modelo0)
```

    ## 
    ## Call:
    ## glm(formula = y_binomial ~ anios_esc, family = binomial("logit"), 
    ##     data = mydata, na.action = na.exclude)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.0243  -1.1094   0.7193   1.0303   1.9401  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -1.71671    0.12090  -14.20   <2e-16 ***
    ## anios_esc    0.17275    0.01113   15.52   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3958.5  on 2857  degrees of freedom
    ## Residual deviance: 3681.8  on 2856  degrees of freedom
    ## AIC: 3685.8
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
confint(modelo0)
```

    ## Waiting for profiling to be done...

    ##                 2.5 %     97.5 %
    ## (Intercept) -1.955894 -1.4818550
    ## anios_esc    0.151151  0.1948061

Con jtools:

``` r
summ(modelo0)
```

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">
<tbody>
<tr>
<td style="text-align:left;font-weight: bold;">
Observations
</td>
<td style="text-align:right;">
2858
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
Dependent variable
</td>
<td style="text-align:right;">
y_binomial
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
Type
</td>
<td style="text-align:right;">
Generalized linear model
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
Family
</td>
<td style="text-align:right;">
binomial
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
Link
</td>
<td style="text-align:right;">
logit
</td>
</tr>
</tbody>
</table>
<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">
<tbody>
<tr>
<td style="text-align:left;font-weight: bold;">
𝛘²(1)
</td>
<td style="text-align:right;">
276.76
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
Pseudo-R² (Cragg-Uhler)
</td>
<td style="text-align:right;">
0.12
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
Pseudo-R² (McFadden)
</td>
<td style="text-align:right;">
0.07
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
AIC
</td>
<td style="text-align:right;">
3685.77
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
BIC
</td>
<td style="text-align:right;">
3697.69
</td>
</tr>
</tbody>
</table>
<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;border-bottom: 0;">
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
Est.
</th>
<th style="text-align:right;">
S.E.
</th>
<th style="text-align:right;">
z val.
</th>
<th style="text-align:right;">
p
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;font-weight: bold;">
(Intercept)
</td>
<td style="text-align:right;">
-1.72
</td>
<td style="text-align:right;">
0.12
</td>
<td style="text-align:right;">
-14.20
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
anios_esc
</td>
<td style="text-align:right;">
0.17
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
15.52
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
</tbody>
<tfoot>
<tr>
<td style="padding: 0; " colspan="100%">
<sup></sup> Standard errors: MLE
</td>
</tr>
</tfoot>
</table>

## Predicción de probabilidades

Para predecir la probabilidad, primero chequemos el rango de nuestra
variabe explicativa

``` r
range(mydata$anios_esc)
```

    ## [1]  0 21

Hacemos un vector con los valores que queremos predecir

``` r
xanios_esc <- 0:25
```

Vamos a utilizar el comando “predict” para predecir los valores. Podemos
el argumento “response” para que nos dé el logito

``` r
y_logito <- predict(modelo0, list(anios_esc = xanios_esc))
y_prob<- predict(modelo0, list(anios_esc = xanios_esc), type= "response")

results_m0<-cbind(y_logito, y_prob, xanios_esc)
results_m0<-as.data.frame(results_m0)
```

Hoy podemos graficar

``` r
ggplot(data=results_m0, aes(x=xanios_esc, y=y_prob)) +
  geom_point()
```

![](logit_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

## Coeficientes exponenciados

Para interpretar mejor los coeficientes suelen exponenciarse y hablar de
las veces que aumentan o disminuyen los momios con respecto a la unidad
como base. Si exponenciamos a ambos lados de nuestra ecuación:

![ e^{ln\\frac{p(x=1)}{p(x=0)}}=e^{\\beta_o+\\beta_1x +\\epsilon}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%20e%5E%7Bln%5Cfrac%7Bp%28x%3D1%29%7D%7Bp%28x%3D0%29%7D%7D%3De%5E%7B%5Cbeta_o%2B%5Cbeta_1x%20%2B%5Cepsilon%7D " e^{ln\frac{p(x=1)}{p(x=0)}}=e^{\beta_o+\beta_1x +\epsilon}")

![ \\frac{p(x=1)}{p(x=0)}=e^{\\beta_o+\\beta_1x +\\epsilon}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%20%5Cfrac%7Bp%28x%3D1%29%7D%7Bp%28x%3D0%29%7D%3De%5E%7B%5Cbeta_o%2B%5Cbeta_1x%20%2B%5Cepsilon%7D " \frac{p(x=1)}{p(x=0)}=e^{\beta_o+\beta_1x +\epsilon}")

Al exponenciar los coeficientes, tenemos los resultados en términos de
momios.

![ \\frac{p}{1-p}=e^{\\beta_o}\*+\*e^{\\beta_1x}\*e^{\\epsilon}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%20%5Cfrac%7Bp%7D%7B1-p%7D%3De%5E%7B%5Cbeta_o%7D%2A%2B%2Ae%5E%7B%5Cbeta_1x%7D%2Ae%5E%7B%5Cepsilon%7D " \frac{p}{1-p}=e^{\beta_o}*+*e^{\beta_1x}*e^{\epsilon}")

Por tantopodemos establecer por cuánto se multiplican los momios de
probabilidad. Lo cual es una manera más sencilla para interpretar
nuestros resultados

``` r
exp(coef(modelo0))
```

    ## (Intercept)   anios_esc 
    ##   0.1796564   1.1885644

Es muy fácil con la librería jtools, sacar los coeficientes
exponenciados. La ventaja es que nos dan también los intervalos:

``` r
summ(modelo0, exp=T )
```

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">
<tbody>
<tr>
<td style="text-align:left;font-weight: bold;">
Observations
</td>
<td style="text-align:right;">
2858
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
Dependent variable
</td>
<td style="text-align:right;">
y_binomial
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
Type
</td>
<td style="text-align:right;">
Generalized linear model
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
Family
</td>
<td style="text-align:right;">
binomial
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
Link
</td>
<td style="text-align:right;">
logit
</td>
</tr>
</tbody>
</table>
<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">
<tbody>
<tr>
<td style="text-align:left;font-weight: bold;">
𝛘²(1)
</td>
<td style="text-align:right;">
276.76
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
Pseudo-R² (Cragg-Uhler)
</td>
<td style="text-align:right;">
0.12
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
Pseudo-R² (McFadden)
</td>
<td style="text-align:right;">
0.07
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
AIC
</td>
<td style="text-align:right;">
3685.77
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
BIC
</td>
<td style="text-align:right;">
3697.69
</td>
</tr>
</tbody>
</table>
<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;border-bottom: 0;">
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
exp(Est.)
</th>
<th style="text-align:right;">
2.5%
</th>
<th style="text-align:right;">
97.5%
</th>
<th style="text-align:right;">
z val.
</th>
<th style="text-align:right;">
p
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;font-weight: bold;">
(Intercept)
</td>
<td style="text-align:right;">
0.18
</td>
<td style="text-align:right;">
0.14
</td>
<td style="text-align:right;">
0.23
</td>
<td style="text-align:right;">
-14.20
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
anios_esc
</td>
<td style="text-align:right;">
1.19
</td>
<td style="text-align:right;">
1.16
</td>
<td style="text-align:right;">
1.21
</td>
<td style="text-align:right;">
15.52
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
</tbody>
<tfoot>
<tr>
<td style="padding: 0; " colspan="100%">
<sup></sup> Standard errors: MLE
</td>
</tr>
</tfoot>
</table>

## Agregando una variable

``` r
modelo1<-glm(y_binomial ~ anios_esc + sex, family = binomial("logit"), data=mydata, na.action=na.exclude)
summary(modelo1)
```

    ## 
    ## Call:
    ## glm(formula = y_binomial ~ anios_esc + sex, family = binomial("logit"), 
    ##     data = mydata, na.action = na.exclude)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.0457  -1.1238   0.7052   1.0530   1.9712  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -1.69350    0.12244 -13.831   <2e-16 ***
    ## anios_esc    0.17401    0.01120  15.544   <2e-16 ***
    ## sexMujer    -0.09468    0.08130  -1.164    0.244    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3958.5  on 2857  degrees of freedom
    ## Residual deviance: 3680.4  on 2855  degrees of freedom
    ## AIC: 3686.4
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
confint(modelo1)
```

    ## Waiting for profiling to be done...

    ##                  2.5 %      97.5 %
    ## (Intercept) -1.9356923 -1.45558854
    ## anios_esc    0.1522976  0.19619498
    ## sexMujer    -0.2541976  0.06456918

Este modelo tiene coeficientes que deben leerse “condicionados”. Es
decir, en este caso tenemos que el coeficiente asociado a la anios_esc,
mantiene constante el sexo y viceversa.

Veamos con los valores exponenciados:

## Bondad de Ajuste

### Devianza

La devianza es una medida de la bondad de ajuste de los modelos lineales
generalizados. O más bien, es una medida de la no-bondad del ajust,
puesto que valores más altos indican un peor ajuste.

R nos da medidas de devianza: la devianza nula y la desviación residual.
La devianza nula muestra qué tan bien la variable de respuesta se
predice mediante un modelo que incluye solo la intersección (gran
media).

### Prueba de Verosimilitud

![image](lrtest.png)

¿Cómo saber si ha mejorado nuestro modelo? Podemos hacer un test que
compare las devianzas(tendría la misma lógica que nuestra prueba F del
modelo lineal). Para esto tenemos que instalar un paquete “lmtest”

``` r
lrtest0<-lrtest(modelo0, modelo1)
lrtest0
```

    ## Warning in knit_print.huxtable(ht): Unrecognized output format "gfm-yaml". Using `to_screen` to print huxtables.
    ## Set options("huxtable.knitr_output_format") manually to "latex", "html", "rtf", "docx", "pptx", "md" or "screen".

                 ┌────────────────────────────────────────────┐
                 │ #Df      LogLik    Df   Chisq   Pr(>Chisq) │
                 ├────────────────────────────────────────────┤
                 │   2   -1.84e+03                            │
                 │   3   -1.84e+03     1    1.36        0.244 │
                 └────────────────────────────────────────────┘

Column names: \#Df, LogLik, Df, Chisq, Pr(\>Chisq)

Como puedes ver, el resultado muestra un valor p muy pequeño (\<.001).
Esto significa que agregar el sexo al modelo lleva a un ajuste
significativamente mejor sobre el modelo original.

Podemos seguir añadiendo variables sólo “sumando” en la función

``` r
modelo2<-glm(y_binomial ~ anios_esc + sex + eda,
             family = binomial("logit"), 
             data=mydata, 
             na.action=na.exclude)
summary(modelo2)
```

    ## 
    ## Call:
    ## glm(formula = y_binomial ~ anios_esc + sex + eda, family = binomial("logit"), 
    ##     data = mydata, na.action = na.exclude)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.1536  -1.0931   0.6532   1.0811   1.9468  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -2.117696   0.189699 -11.163  < 2e-16 ***
    ## anios_esc    0.183016   0.011679  15.671  < 2e-16 ***
    ## sexMujer    -0.107889   0.081551  -1.323  0.18585    
    ## eda          0.008970   0.003025   2.965  0.00303 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3958.5  on 2857  degrees of freedom
    ## Residual deviance: 3671.6  on 2854  degrees of freedom
    ## AIC: 3679.6
    ## 
    ## Number of Fisher Scoring iterations: 4

Y podemos ver si introducir esta variable afectó al ajuste global del
modelo

``` r
lrtest1<-lrtest(modelo1, modelo2)
lrtest1
```

    ## Warning in knit_print.huxtable(ht): Unrecognized output format "gfm-yaml". Using `to_screen` to print huxtables.
    ## Set options("huxtable.knitr_output_format") manually to "latex", "html", "rtf", "docx", "pptx", "md" or "screen".

                 ┌────────────────────────────────────────────┐
                 │ #Df      LogLik    Df   Chisq   Pr(>Chisq) │
                 ├────────────────────────────────────────────┤
                 │   3   -1.84e+03                            │
                 │   4   -1.84e+03     1    8.84      0.00294 │
                 └────────────────────────────────────────────┘

Column names: \#Df, LogLik, Df, Chisq, Pr(\>Chisq)

### Test Hosmer-Lemeshow Goodness of Fit “GOF”

El teste Homer-Lemeshow se calcula sobre los datos una vez que las
observaciones se han segmentado en grupos basados en probabilidades
predichas similares. Este teste examina si las proporciones observadas
de eventos son similares a las probabilidades predichas de ocurrencia en
subgrupos del conjunto de datos, y lo hace con una prueba de chi
cuadrado de Pearson.

¡Ojo! No queremos rechazar la hipótesis nula. La hipótesis nula sostiene
que el modelo se ajusta a los datos por lo tanto no queremos rechazarla.

``` r
hoslem.test(mydata$y_binomial, fitted(modelo2))
```

    ## 
    ##  Hosmer and Lemeshow goodness of fit (GOF) test
    ## 
    ## data:  mydata$y_binomial, fitted(modelo2)
    ## X-squared = 8.2875, df = 8, p-value = 0.4059

No obstante, esta prueba ha sido criticada. Checa la postura de Paul
Allison <https://statisticalhorizons.com/hosmer-lemeshow>

Es un problema que tenemos en muestras grandes. Casi siempre preferimos
el enfoque de la devianza.

## Tabla de modelos estimados

``` r
#stargazer(modelo0, modelo1,modelo2, type = 'latex', header=FALSE)
```

``` r
stargazer(modelo0, modelo1,modelo2, 
          type = 'text', header=FALSE)
```

    ## 
    ## ==================================================
    ##                         Dependent variable:       
    ##                   --------------------------------
    ##                              y_binomial           
    ##                      (1)        (2)        (3)    
    ## --------------------------------------------------
    ## anios_esc          0.173***   0.174***   0.183*** 
    ##                    (0.011)    (0.011)    (0.012)  
    ##                                                   
    ## sexMujer                       -0.095     -0.108  
    ##                               (0.081)    (0.082)  
    ##                                                   
    ## eda                                      0.009*** 
    ##                                          (0.003)  
    ##                                                   
    ## Constant          -1.717***  -1.693***  -2.118*** 
    ##                    (0.121)    (0.122)    (0.190)  
    ##                                                   
    ## --------------------------------------------------
    ## Observations        2,858      2,858      2,858   
    ## Log Likelihood    -1,840.886 -1,840.207 -1,835.785
    ## Akaike Inf. Crit. 3,685.771  3,686.414  3,679.570 
    ## ==================================================
    ## Note:                  *p<0.1; **p<0.05; ***p<0.01

Para sacar los coeficientes exponenciados

``` r
stargazer(modelo0, modelo1,modelo2, 
          type = 'text', header=FALSE,
          apply.coef = exp,
          apply.se   = exp)
```

    ## 
    ## ==================================================
    ##                         Dependent variable:       
    ##                   --------------------------------
    ##                              y_binomial           
    ##                      (1)        (2)        (3)    
    ## --------------------------------------------------
    ## anios_esc           1.189      1.190      1.201   
    ##                    (1.011)    (1.011)    (1.012)  
    ##                                                   
    ## sexMujer                       0.910      0.898   
    ##                               (1.085)    (1.085)  
    ##                                                   
    ## eda                                       1.009   
    ##                                          (1.003)  
    ##                                                   
    ## Constant            0.180      0.184      0.120   
    ##                    (1.129)    (1.130)    (1.209)  
    ##                                                   
    ## --------------------------------------------------
    ## Observations        2,858      2,858      2,858   
    ## Log Likelihood    -1,840.886 -1,840.207 -1,835.785
    ## Akaike Inf. Crit. 3,685.771  3,686.414  3,679.570 
    ## ==================================================
    ## Note:                  *p<0.1; **p<0.05; ***p<0.01

Veamos con jtools:

``` r
export_summs(modelo0, modelo1,modelo2, exp=T)
```

    ## Warning in knit_print.huxtable(x, ...): Unrecognized output format "gfm-yaml". Using `to_screen` to print huxtables.
    ## Set options("huxtable.knitr_output_format") manually to "latex", "html", "rtf", "docx", "pptx", "md" or "screen".

           ─────────────────────────────────────────────────────────
                             Model 1       Model 2       Model 3    
                         ───────────────────────────────────────────
             (Intercept)      0.18 ***      0.18 ***      0.12 ***  
                             (0.12)        (0.12)        (0.19)     
             anios_esc        1.19 ***      1.19 ***      1.20 ***  
                             (0.01)        (0.01)        (0.01)     
             sexMujer                       0.91          0.90      
                                           (0.08)        (0.08)     
             eda                                          1.01 **   
                                                         (0.00)     
                         ───────────────────────────────────────────
             N             2858          2858          2858         
             AIC           3685.77       3686.41       3679.57      
             BIC           3697.69       3704.29       3703.40      
             Pseudo R2        0.12          0.12          0.13      
           ─────────────────────────────────────────────────────────
             *** p < 0.001; ** p < 0.01; * p < 0.05.                

Column names: names, Model 1, Model 2, Model 3

También la librería “sjPlot” tiene el comando “plot_model()”

``` r
plot_model(modelo2)
```

![](logit_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

Por default nos da los coeficientes exponenciados.

¿Cómo saber lo que tiene esos gráficos? Es bueno guardar siempre estos
resultados en un objeto. Este objeto es una lista de dos listas

``` r
get<-plot_model(modelo2)
get$data
```

    ## Warning in knit_print.huxtable(ht): Unrecognized output format "gfm-yaml". Using `to_screen` to print huxtables.
    ## Set options("huxtable.knitr_output_format") manually to "latex", "html", "rtf", "docx", "pptx", "md" or "screen".

┌─────────────────────────────────────────────────────────────────────────────
│ term estimate std.erro conf.lev conf.low conf.hig statisti  
│ r el h c  
├─────────────────────────────────────────────────────────────────────────────
│ anios_es 1.2   0.0117  0.95 1.17  1.23 15.7   
│ c  
│ sexMujer 0.898 0.0816  0.95 0.765 1.05 -1.32  
│ eda 1.01  0.00303 0.95 1     1.02 2.97  
└─────────────────────────────────────────────────────────────────────────────

Column names: term, estimate, std.error, conf.level, conf.low,
conf.high, statistic, df.error, p.value, p.stars, p.label, group, xpos,
xmin, xmax

7/15 columns shown.

``` r
plot_model(modelo2, terms= c("anios_esc", "sex"), type="pred")
```

    ## Data were 'prettified'. Consider using `terms="anios_esc [all]"` to get smooth plots.

![](logit_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

Para poner más de un modelo:

``` r
plot_models(modelo1, modelo2) + ggtitle("P(acceso a servicios médicos)")
```

![](logit_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

# Regresión Probit

## Un solo predictor

``` r
mprobit<-glm(y_binomial ~ anios_esc, family = binomial("probit"), data=mydata, na.action=na.exclude)
summary(mprobit)
```

    ## 
    ## Call:
    ## glm(formula = y_binomial ~ anios_esc, family = binomial("probit"), 
    ##     data = mydata, na.action = na.exclude)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.0558  -1.1101   0.7186   1.0338   1.9617  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -1.053709   0.072241  -14.59   <2e-16 ***
    ## anios_esc    0.105924   0.006595   16.06   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3958.5  on 2857  degrees of freedom
    ## Residual deviance: 3682.1  on 2856  degrees of freedom
    ## AIC: 3686.1
    ## 
    ## Number of Fisher Scoring iterations: 4

Comparando probit con logit:

``` r
stargazer(modelo0, mprobit,   type = 'text', header=FALSE)
```

    ## 
    ## ==============================================
    ##                       Dependent variable:     
    ##                   ----------------------------
    ##                            y_binomial         
    ##                      logistic       probit    
    ##                        (1)            (2)     
    ## ----------------------------------------------
    ## anios_esc            0.173***      0.106***   
    ##                      (0.011)        (0.007)   
    ##                                               
    ## Constant            -1.717***      -1.054***  
    ##                      (0.121)        (0.072)   
    ##                                               
    ## ----------------------------------------------
    ## Observations          2,858          2,858    
    ## Log Likelihood      -1,840.886    -1,841.052  
    ## Akaike Inf. Crit.   3,685.771      3,686.104  
    ## ==============================================
    ## Note:              *p<0.1; **p<0.05; ***p<0.01

¿Cuál es la diferencia?

<https://tutorials.methodsconsultants.com/posts/what-is-the-difference-between-logit-and-probit-models>.

Y Allison:

<https://statisticalhorizons.com/in-defense-of-logit-part-1>
<https://statisticalhorizons.com/in-defense-of-logit-part-2>
