#Proyecto afinia
kwh <- c(3375, 2661, 2073, 2579, 2858, 2296, 2812, 2433, 2266, 3128) 
grado <- c(2421, 1841, 438, 15, 152, 1028, 1967, 1627, 537, 26)  
temperatura <- c(26, 34, 58, 72, 67, 48, 33, 39, 66, 71)  
costo <- c(321.94, 221.11, 205.16, 251.07, 279.8, 183.84, 244.93, 218.59, 213.09, 333.49)  
datos <- data.frame(kwh, grado,temperatura, costo)

#matriz de correlación entre las variables numéricas de tus datos y la visualizas utilizando
correlacion <- cor(datos)
correlacion
library(corrplot)
corrplot(correlacion,method = "number",type="lower")
#Observamos aun correlación muy alta con un valor de 0.92 entre el costo y kwh

# Ajustar el modelo de regresión lineal
modelo1<-lm(costo ~ kwh , data=datos)

# Mostrar los coeficientes
summary(modelo1)
#Al aplicar el summary del modelo podemos observar un P p-value: 0.0002059 el cual
# nos indica que existe un la relacion y que una variable influye en la otra.

#como el p valor del  intercepto es mayor al 5% de significancia no lo tenemos en cuenta.
p_intercepto <- summary(modelo1)$coefficients["(Intercept)", "Pr(>|t|)"]
if( p_intercepto > 0.5){
  #Nuevo modelo sin intercepto
  modelo1<-lm(costo ~ kwh -1, data=datos)
  summary(modelo1)
}
#valores asociados a las pruebas de hipótesis, coeficientes son los valores de 
#la prueba incluye intercepto y coeficiente de la funcion
# Tomar el valor del intercepto
#intercepto <- coef(modelo_simple)[1]
#intercepto

# Tomar el valor del coeficiente
pendiente <- coef(modelo1)[1]
pendiente

#Tomamos el valor del porcentaje de variacion ajustado
porcentajeVariacion <- summary(modelo1)$adj.r.squared 
porcentajeVariacion

#Con los valores dados planteamos la ecuación de la recta
#Ecuacion = pendiente*kwh
#Para una 
costo = pendiente*3000
costo

#-------------Multiple--------------------------------------
modelo_simple<-lm(costo ~ kwh + temperatura , data=datos)
summary(modelo_simple)


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
bptest(modelo_simple, ~ kwh + temperatura)
help(bptest)

# SUPUESTO 3: Linealidad
# Análisis gráfico y coeficiente de correlación
y_estimados <- predict(modelo_simple)
plot(y_estimados, datos$costo)
abline(a = 0, b = 1, col = "red")
cor(y_estimados, datos$costo)

# SUPUESTO 4: Independencia de los errores
# Análisis gráfico y prueba de Durbin-Watson
library(lmtest)
residuales <- residuals(modelo_simple)
plot(residuales)
#Test de Durbin-Watson
#H0:Los errores son independientes
dwtest(modelo_simple, alternative = "two.sided")
help(dwtest)
