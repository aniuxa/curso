#============================================================================#
#Fecha: 2022-06-10 # Sesión práctica
#Autora: Ana Escoto
# ============================================================================#



# Paquetes ----

#install.packages("sjPlot", dependencies=T) # solito porque da problmas
library(sjPlot)

if (!require("pacman")) install.packages("pacman") # instala pacman si se requiere
pacman::p_load(tidyverse, # sobretodo para dplyr
              haven, #importación
              janitor, #tablas
              sjlabelled, # etiquetas
              DescTools, # Paquete para estimaciones y pruebas
              infer, # tidy way 
              broom,  # Una escobita para limpiar (pero es para arreglar)
              estimatr, car, stargazer, ggpubr, 
              jtools, lm.beta, robustbase, sandwich,
              officer,flextable,huxtable, ggstance, kableExtra) # Para la regresión




# Datos #
SDEMT122 <- read_dta("datos/SDEMT122.dta") 

mydata<- SDEMT122 %>% 
  filter(clase2==1) %>%  # me quedo con la población ocupada
  filter(ing_x_hrs>0) %>% # ingresos válidso
  filter(anios_esc<99) %>% # quito missings anios de escolaridad
  filter(eda>14 & eda<99) %>% #PET
  select(eda, sex, anios_esc, ing_x_hrs, pos_ocu, imssissste, medica5c, ent)  
  
tail(mydata)

# Repaso regresión lineal simple ----
# Relaciones

mydata %>% 
  ggplot()+ 
  aes(anios_esc, log(ing_x_hrs)) +
        geom_jitter()

mydata$log_ing_x_hrs<-log(mydata$ing_x_hrs)

cor(mydata$log_ing_x_hrs, mydata$anios_esc,  use = "pairwise")



## Relación lineal e inferencia ----

cor_test<-mydata %>% 
  with(
    cor.test(log_ing_x_hrs, anios_esc, use="pairwise.complete.obs")
  )

#dos modos de visualizar el resultado
cor_test 
tidy(cor_test)



## Revisando logaritmo ----

hist(log(mydata$ing_x_hrs))


## Ajuste modelo simple ----

modelo <-lm(log_ing_x_hrs ~anios_esc, data=mydata, 
            na.action=na.exclude)

summary(modelo) # resultados


tidy(modelo) # Pruebas de hipótesis de los coeficientes
confint(modelo)
glance(modelo) # resultado ajuste global
anova(modelo)
plot(modelo)



##  Outliers ----
outlierTest(modelo) # Bonferonni p-value for most extreme obs

out<-outlierTest(modelo) # guardamos en objeto


## qqPlot ----
qqPlot<-qqPlot(modelo)
ggpubr::ggqqplot(mydata$log_ing_x_hrs)
car::qqPlot(modelo, main="QQ Plot") #qq plot for studentized resid


## Heterocedasticidad

# non-constant error variance test
ncvTest(modelo)

# plot studentized residuals vs. fitted values 
spreadLevelPlot(modelo)


## corrección modelo ----
names(out$bonf.p)
outliers<-rbind(as.integer(names(out$bonf.p)), qqPlot) # lista los casos 

mydata$rownames<-rownames(mydata)
#View(mydata) # verificamos que no hayamos movido el orden

mydata2<-mydata[-outliers,]

#Modelo nuevo 
modelo0<-lm(log_ing_x_hrs ~anios_esc, data=mydata2, na.action=na.exclude)
summary(modelo0)

qqPlot(modelo0)
outlierTest(modelo0)


# Una variable categórica + ----

## Gráfico ----

mydata %>% 
  ggplot()+ 
  aes(anios_esc,
      log(ing_x_hrs),
      color=as_label(sex)) +
  geom_jitter() +
  geom_smooth(method="lm")+
  facet_wrap(vars(as_label(sex)))



modelo1<-lm(log_ing_x_hrs ~anios_esc + as_label(sex), data=mydata, na.action=na.exclude)
summary(modelo1)


pruebaf0<-anova(modelo, modelo1)
pruebaf0

## Una variable continua ----
modelo2<-lm(log_ing_x_hrs ~ anios_esc + as_label(sex) + eda, 
            data=mydata, 
            na.action=na.exclude)
summary(modelo2)


pruebaf1<-anova(modelo1, modelo2)
pruebaf1


vif(modelo2) #Multicolinealidad

#stargazer(modelo0, modelo1,modelo2, type = 'latex', header=FALSE)
stargazer(modelo, modelo1,modelo2, type = 'text', header=FALSE)

# Gráficos de modelos  SjPlot ----
plot_model(modelo1)
plot_models(modelo, modelo1, modelo2)


# Tablasde modelo ----


tidy(modelo2)%>%
  kbl() %>%
  kable_paper("hover", full_width = F)

# Estandarizar ####
lm.beta(modelo2)


modelo_beta<-lm.beta(modelo2)
modelo_beta

plot_model(modelo2, type="std")

#  modificando la variable de factor 
mydata<-mydata %>% 
  mutate(sex=as_label(sex))

modelo2<-lm(log_ing_x_hrs ~ anios_esc +sex + eda, data=mydata)

summary(modelo2)

## Efectos ----
sjPlot::plot_model(modelo2, type="pred", terms = "anios_esc")
plot_model(modelo2, type="pred", terms = c("anios_esc","sex")) + theme_blank()
plot_model(modelo2, type="pred", terms = c("sex","anios_esc")) + theme_blank()
plot_model(modelo2, type="eff", terms = "anios_esc")
plot_model(modelo2, type="eff", terms = "sex")


eff<-plot_model(modelo2, type="eff", terms = "anios_esc")
eff$data
eff<-plot_model(modelo2, type="pred", terms = "anios_esc")
eff$data


# Introducción a las interacciones ----

modelo_int1<-lm(log_ing_x_hrs ~ anios_esc * sex , data = mydata, na.action=na.exclude)
summary(modelo_int1)

plot_model(modelo_int1, type="int", terms = c("sex", "anios_esc"))


# Explicitando el logaritmo ----

mydata2<-mydata %>% filter(anios_esc>0)

modelo_log<-lm(log(ing_x_hrs) ~ log(anios_esc) + sex,
               data=mydata2, 
               na.action = na.exclude)

summary(modelo_log)

plot_model(modelo_log, type="pred", terms ="anios_esc")



# Cuadrático ----

modelo_quadr<-lm(log_ing_x_hrs ~ anios_esc + I(anios_esc^2) + sex, 
                 data=mydata, 
                 na.action=na.exclude)
summary(modelo_quadr)

plot_model(modelo_quadr, type="pred", terms = c("anios_esc"))


# Regresión robusta ----

modelo2rob1 <- lm_robust(log_ing_x_hrs ~ anios_esc + as_label(sex) + eda, data = mydata)

summary(modelo2rob1)
tidy(modelo2rob1)

# Regresión con errores robustos

modelo2rob2<- lm_robust(log_ing_x_hrs ~ anios_esc + as_label(sex) + eda, data = mydata, clusters = ent)
summary(modelo2rob2)
summ(modelo2, robust = "HC1")
summ(modelo2, scale = TRUE)


modelo2rob3<-lmrob(log_ing_x_hrs ~ anios_esc + as_label(sex) + eda, data = mydata, 
    na.action = na.exclude)
summary(modelo2rob3)


stargazer(modelo2, modelo2rob3, type = 'text', header=FALSE)

jtools:::export_summs(modelo2, modelo2rob1, modelo2rob2, modelo2rob3)


#jtools::export_summs(modelo2, modelo2rob1, modelo2rob2, modelo2rob3, to.file = "PDF", file.name = "test.pdf")

plot_summs(modelo2,
          scale=T,
          plot.distributions = TRUE, 
          inner_ci_level = .9)

