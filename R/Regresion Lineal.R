#CARGANDO PAQUETES
library(ggplot2)
library(corrplot)
library(dplyr)
library(readxl)
#LLAMAMOS LOS DATOS SOLO VAR NUMÉRICAS 
datos<-read_xlsx("C:/Users/tito_/OneDrive - cecar.edu.co/R/datos_energia.xlsx")
datos

#calculas la matriz de correlación entre las variables numéricas de tus datos y la visualizas utilizando
correlacion <- cor(datos)
correlacion
corrplot(correlacion,method = "number",type="lower")

#diagrama de dispercion entre  temperatura y consumo de energia
plot(x=datos$`Temperatura (°C)`,y=datos$`Consumo de energía`)

#Aplicamos el modelo
modelo <- lm(datos$`Consumo de energía`~datos$`Temperatura (°C)`)
modelo
summary(modelo)
plot(modelo)
#valores asociados a las pruebas de hipótesis, coeficientes son los valores de 
#las pruebas incluye intercepto y coeficiente de la funcion
# Tomar el valor del intercepto
intercepto <- coef(modelo)[1]
intercepto
# Tomar el valor del coeficiente para 'Temperatura'
pendiente <- coef(modelo)[2]
pendiente

#tambien tenemos 2 pruebas de hipótesis asociativas 
#para b=intercepto H0: b=0, H1: b!=0
#para m= pendiente H1: m=0, H1: m!=0
#el p valor del F static debe ser muy pequeño para que nos diga que
#existe un la relacion y que una variable influye a la otra
#realizamos ACP  analisis de componentes principales

#Modelo 2 
#CARGANDO PAQUETES
library(ggplot2)
library(corrplot)
library(dplyr)
library(readxl)
#LLAMAMOS LOS DATOS SOLO VAR NUMÉRICAS 
data("mtcars")
datos<-mtcars
datos

iteracion = 0;

while (TRUE){
  
  iteracion = iteracion +1
  cat("Iteración:", iteracion, "\n")
  modelo<-lm(mpg ~ .,data = datos)
  summary_modelo<-summary(modelo)
  cat("existe \n")


  #buscamos los coeficientes del modelo
  p_valores <- summary_modelo$coefficients[, "Pr(>|t|)"]
  #buscamos la variable con el p valor maximo y su coeficiente 
  p_valor_max<-max(p_valores)
  cat("P valor máximo:", p_valor_max, "\n")
  
  
  # Si el máximo p-valor es mayor a 0.05 y no es 'mpg'
  if (p_valor_max > 0.05 && which.max(p_valores) != 1){
    indice_max_p_valor<-which.max(p_valores)
    cat("Indice P valor máximo:", indice_max_p_valor, "\n")
    cat("Variable eliminada",colnames(datos)[indice_max_p_valor],"\n")
    # le actualizamos el modelo
    datos<- datos[,-indice_max_p_valor]

  }else {
     # caso contrario  todos los p valor son mayores que 0.05 cerramos el while 
    # toca preguntar si p valor del intercepto 
    modelo<-lm(mpg ~ -1 + .,data = datos)
    summary(modelo)
    break
  }
}
summary_modelo


library(carData)
data(Salaries)
str(Salaries)
help("Salaries")
ajuste1<-lm(salary~ .,data = Salaries)
ajuste1

iteracion = 0;

while (TRUE){
  
  iteracion = iteracion +1
  cat("Iteración:", iteracion, "\n")
  modelo<-lm(mpg ~ .,data = datos)
  summary_modelo<-summary(modelo)
  cat("existe \n")
  
  
  #buscamos los coeficientes del modelo
  p_valores <- summary_modelo$coefficients[, "Pr(>|t|)"]
  #buscamos la variable con el p valor maximo y su coeficiente 
  p_valor_max<-max(p_valores)
  cat("P valor máximo:", p_valor_max, "\n")
  
  
  # Si el máximo p-valor es mayor a 0.05 y no es 'mpg'
  if (p_valor_max > 0.05 && which.max(p_valores) != 1){
    indice_max_p_valor<-which.max(p_valores)
    cat("Indice P valor máximo:", indice_max_p_valor, "\n")
    cat("Variable eliminada",colnames(datos)[indice_max_p_valor],"\n")
    # le actualizamos el modelo
    datos<- datos[,-indice_max_p_valor]
    
  }else {
    # caso contrario  todos los p valor son mayores que 0.05 cerramos el while 
    break
  }
}
summary_modelo





#Grillos y temperatura
# Datos de ejemplo para regresión lineal simple
x_simple <- c(69.7, 93.3, 84.3, 76.3, 88.6, 82.6, 71.6, 79.6) 
y_simple <- c(882, 1188, 1104, 864, 1200, 1032, 960, 900)  
datos <- data.frame(x_simple, y_simple)

#calculas la matriz de correlación entre las variables numéricas de tus datos y la visualizas utilizando
correlacion <- cor(datos)
correlacion
library(corrplot)
corrplot(correlacion,method = "number",type="lower")
#Observamos una correlación muy alta con un valor de 0.87 entre las variables.

#diagrama de dispercion entre  las variables
plot(x=x_simple,y=y_simple)
#Observamos una distribución aproximadamente lineal creciente entre las variables.

# Ajustar el modelo de regresión lineal simple
modelo_simple <- lm(y_simple ~ x_simple)
plot(modelo_simple)


# Mostrar los coeficientes
summary(modelo_simple)
#Al aplicar el summary del modelo podemos observar un P valor de 0.00457 el cual
# nos indica que existe un la relacion y que una variable influye a la otra.

#como el p valor del  intercepto es mayor al 5% de significancia no lo tenemos en cuenta.
p_intercepto <- summary(modelo_simple)$coefficients["(Intercept)", "Pr(>|t|)"]
if( p_intercepto > 0.5){
  modelo_simple <- lm(y_simple ~ x_simple-1)
  summary(modelo_simple)
}

#valores asociados a las pruebas de hipótesis, coeficientes son los valores de 
#la prueba incluye intercepto y coeficiente de la funcion
# Tomar el valor del intercepto
#intercepto <- coef(modelo_simple)[1]
#intercepto

# Tomar el valor del coeficiente
pendiente <- coef(modelo_simple)[1]
pendiente

#Tomamos el valor del porcentaje de variacion ajustado
porcentajeVariacion <- summary(modelo_simple)$adj.r.squared
porcentajeVariacion

#Con los valores dados planteamos la ecuación de la recta
#Ecuacion = pendiente*Temperatura
#Para una temperatura de 75°
ChirridosPorMinutos = pendiente*75
ChirridosPorMinutos

########################################
######VERIFICAMOS LOS SUPUESTOS#########
########################################

#Analizar los residuales del modelo

#SUPUESTO 1
#Analizar la normalidad
residuos_estan<-rstandard(modelo_simple)
#Análisis grafico
par(mfrow=c(1,2))
qqnorm(residuos_estan)
qqline(residuos_estan)
hist(residuos_estan)
#Test de Normalidad
shapiro.test(residuos_estan)

# SUPUESTO 2: Varianza constante de los residuos
# Análisis gráfico y prueba de homocedasticidad
plot(modelo_simple, which = 1)
library(zoo)
library(lmtest)
#bptest(modelo_simple)
bptest(modelo_simple, ~ x_simple)
help(bptest)

# SUPUESTO 3: Linealidad
# Análisis gráfico y coeficiente de correlación
y_estimados <- predict(modelo_simple)
plot(y_estimados, y_simple)
abline(a = 0, b = 1, col = "red")
cor(y_estimados, y_simple)


# SUPUESTO 4: Independencia de los errores
# Análisis gráfico y prueba de Durbin-Watson
library(lmtest)
residuales <- residuals(modelo_simple)
plot(residuales)
#Test de Durbin-Watson
#H0:Los errores son independientes
dwtest(modelo_simple, alternative = "two.sided")

