# ==============================================================================
# Fecha: 2022-01-10 
# Paquetes a utilizar
# Autora: Ana Escoto
# ==============================================================================


# Paquetes
if (!require("pacman")) install.packages("pacman") # instala pacman si se requiere
pacman::p_load(tidyverse, # conjunto de paquetes tidy
               broom, # paquete para adecuar resultados estadísticos
               car, # para la regresión lineal
               DescTools, #caja de herramientas estadísticas
               esquisse, # para hacer ggplot con drag and drop
               estimatr, # Fast Estimators for Design-Based Inference
               extdplyr, # extensión de dplyr
               ggpubr, # Extensión de ggplot
               gtsummary, # para mostrar resultados
               haven, # importa archivos desde formatos .dta y .sav
               janitor,# para tabulado y limpieza de nombres
               infer, # tibble format de inferencias
               magrittr, # para algunos pipes
               modelsummary, # para resultados de modelos
               gt, # grammar of tables
               kableExtra, # para publicar
               lm.beta, # para coeficientes beta
               RColorBrewer, #paletas de color
               wesanderson, #paletas de color películas Wes Anderson
               robustbase, # Para estimaciones de modelos robustos
               sjlabelled, #manejo de etiquetas y edición
               sjPlot, #graficos de estimaciones de modelos 
               stargazer, #Tablas de estimaciones de modelos
               srvyr # diseño muestral
)
