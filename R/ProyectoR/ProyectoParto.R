#Iniciamos un analisis exploratorio de los datos Pima Indians Diabetes Database
#Llamamos los datos del archivo Excel
datos_parto <- read.csv("C:/Users/tito_/OneDrive - cecar.edu.co/R/ProyectoR/datos_parto.csv")
# Verificar los nombres de las columnas en datos_parto
colnames(datos_parto)
#Bajo criterio del ingeniero se toman las variables significativas para
#entrenar el modelo logistico 
datos_parto_modelo <- datos_parto[c("Género", "Peso", "Talla", "Tiempo.de.Gestación","Tipo.de.Parto",
                                    "Multiplicidad.de.Embarazo","Grupo.Sanguíneo",
                                    "Factor.RH","Edad.de.la.Madre","Número.de.Hijos.Nacidos.Vivos",
                                    "Fecha.Anterior.del.Hijo.Nacido.Vivo","Número.de.Embarazos",
                                    "Nombre.de.la.Administradora")]

#Como observamos de los 1432 datos tenemos 1 que el tipo de parto es instrumentado
#al investigar entendemos que es un parto natural pero con un poco de ayuda
#para poder manejar 2 tipos de resultados y aplicar regresion logistica eliminamos este dato
datos_parto_modelo <- datos_parto_modelo[- which(datos_parto_modelo$Tipo.de.Parto == "INSTRUMENTADO"), ]
# Eliminar las filas donde Factor.RH es vacío
datos_parto_modelo <- datos_parto_modelo[- which(datos_parto_modelo$Factor.RH == ""), ]

str(datos_parto_modelo)
#convertimos los datos de tipo char a factor 
datos_parto_modelo$Género<-as.factor(datos_parto_modelo$Género)
datos_parto_modelo$Tipo.de.Parto<-as.factor(datos_parto_modelo$Tipo.de.Parto)
# Verificar los niveles de la variable "Tipo.de.Parto"
levels(datos_parto_modelo$Tipo.de.Parto)
#identificando como cesaria = 0 y natural = 1
datos_parto_modelo$Multiplicidad.de.Embarazo<-as.factor(datos_parto_modelo$Multiplicidad.de.Embarazo)
datos_parto_modelo$Grupo.Sanguíneo<-as.factor(datos_parto_modelo$Grupo.Sanguíneo)
datos_parto_modelo$Factor.RH<-as.factor(datos_parto_modelo$Factor.RH)
datos_parto_modelo$Nombre.de.la.Administradora<-as.factor(datos_parto_modelo$Nombre.de.la.Administradora)

#Convertir la fecha en tipo date 
datos_parto_modelo$Fecha.Anterior.del.Hijo.Nacido.Vivo <- as.Date(datos_parto_modelo$Fecha.Anterior.del.Hijo.Nacido.Vivo, format = "%d/%m/%Y")
#Luego para tener un mejor manejo vamos a manejar los dias que hay entre
#la fecha de nacimiento y la fecha actual en caso del data set es de 2017
datos_parto_modelo$Fecha.Anterior.del.Hijo.Nacido.Vivo <- as.integer(as.Date('2017-01-01')- datos_parto_modelo$Fecha.Anterior.del.Hijo.Nacido.Vivo)
#Pasamos los NA  a 0 para una mejor interpretacion del modelo
datos_parto_modelo$Fecha.Anterior.del.Hijo.Nacido.Vivo[is.na(datos_parto_modelo$Fecha.Anterior.del.Hijo.Nacido.Vivo)] <- 0

#Verificamos nuevamente el tipo de variable de las columnas
str(datos_parto_modelo)
#observamos los datos del modelo
summary(datos_parto_modelo)
head(datos_parto_modelo)


#analizamos la distribucion de los datos
library(ggplot2)
library(gridExtra)
#Grafico de cajas 
boxplots <- list(
  ggplot(datos_parto_modelo, aes(x = 0, y = Peso )) + geom_boxplot() + geom_jitter(),
  ggplot(datos_parto_modelo, aes(x = 0, y = Talla)) + geom_boxplot() + geom_jitter(),
  ggplot(datos_parto_modelo, aes(x = 0, y = Tiempo.de.Gestación)) + geom_boxplot() + geom_jitter(),
  ggplot(datos_parto_modelo, aes(x = 0, y = Edad.de.la.Madre)) + geom_boxplot() + geom_jitter(),
  ggplot(datos_parto_modelo, aes(x = 0, y = Número.de.Hijos.Nacidos.Vivos)) + geom_boxplot() + geom_jitter())
grid.arrange(grobs = boxplots, ncol = 2)

#Procedemos al ajuste del modelo logistico de tipo de embarazo mediante glm 
modelo1<-glm(Tipo.de.Parto ~ . , data=datos_parto_modelo,family = binomial)
summary(modelo1)
#Analisando los resultados de del modelo y logramos observar que algunos
#coeficientes absolutos de z value para algunas variables son mayores a 1.96 esto
#nos indica significancia estadistica del modelo 

#Dado que la deviance residual es menor que la deviance nula y el AIC es relativamente
#bajo, estos son indicios de que el modelo logístico está proporcionando un buen ajuste a los datos.
#Procedemos a hacer un proceso de seleccion hacia atras Backward_step
library(MASS)
backward_step<- stepAIC(modelo1,direction = "backward")

#El resultado del proceso nos sugiere que el modelo mas apropiado 
#es usando las siguientes variables
#Df Deviance    AIC
#<none>                                     1720.2 1732.2
#- Fecha.Anterior.del.Hijo.Nacido.Vivo  1   1725.4 1735.4
#- Número.de.Hijos.Nacidos.Vivos        1   1736.2 1746.2
#- Edad.de.la.Madre                     1   1741.8 1751.8
#- Tiempo.de.Gestación                  1   1755.6 1765.6
#- Multiplicidad.de.Embarazo            1   1756.2 1766.2

# Actualizar datos_parto_modelo para incluir solo las columnas seleccionadas
datos_parto_modelo_final <- datos_parto_modelo[, c("Tipo.de.Parto", "Tiempo.de.Gestación", "Multiplicidad.de.Embarazo", 
                                             "Edad.de.la.Madre", "Número.de.Hijos.Nacidos.Vivos", 
                                             "Fecha.Anterior.del.Hijo.Nacido.Vivo")]

#Hacemos el modelo logistico  final, con las variables sugeridas  
modelo_final<-glm(Tipo.de.Parto ~ .,data=datos_parto_modelo_final,family = binomial)

#Verificacion de la significancia del modelo
deviance.residual<-modelo_final$deviance
deviance.nulo<-modelo_final$null.deviance
estadistico_chi2<-deviance.nulo-deviance.residual
grados_libertad_chi<-modelo_final$df.null - modelo_final$df.residual
p_valor<-1-pchisq(estadistico_chi2,df=grados_libertad_chi)
p_valor

#verificar la significancia de cada variable 
summary(modelo_final)
# al observar los datos tenemos  un modelo aceptable donde todos los valores 
#absolutos del Z value son mayores a 1.96 (para un nivel de significancia del 5%

#Evaluando el ajuste del modelo
#Pseudo R^2 de McFadden
R2MF<-(deviance.nulo-deviance.residual)/deviance.nulo
R2MF*100

#R^2 de Cox y Snell
N<-dim(datos_parto_modelo_final)[1]
R2CS<-1-exp(1/N*(deviance.residual-deviance.nulo))
#R^2 de Nagelkerke
R2N<-R2CS/(1-exp(-deviance.nulo/N))
R2N*100

#Matriz de confusión
library(gmodels)
predict.fit<-modelo_final$fitted.values
set.seed(123)
predict.fit[predict.fit>=0.5]<-1
predict.fit[predict.fit<0.5]<-0
matriz_confusion<-CrossTable(datos_parto_modelo_final$Tipo.de.Parto,predict.fit,prop.chisq = FALSE,prop.r = F,prop.c = F)

matriz_confusion$prop.tbl
# Acertibilidad del modelo
acertibilidad <- sum(diag(matriz_confusion$prop.tbl)) / sum(matriz_confusion$prop.tbl)
acertibilidad

#Tomamos La sensibilidad que es la mas importante
#(también conocida como la tasa de verdaderos positivos)
sensibilidad <- matriz_confusion$prop.row[2,2]
sensibilidad

#tomamos La especificidad (también conocida como la tasa de verdaderos negativos) 
especificidad <- matriz_confusion$prop.row[1,1]
especificidad

#curvas ROC
library(Epi)
# Calcula la curva ROC utilizando las variables predictoras
ROC(data = datos_parto_modelo_final, form = Tipo.de.Parto ~ Tiempo.de.Gestación + Multiplicidad.de.Embarazo + Edad.de.la.Madre + Número.de.Hijos.Nacidos.Vivos + Fecha.Anterior.del.Hijo.Nacido.Vivo)

#Con probabilidades predichas
ROC(data = datos_parto_modelo_final, form = Tipo.de.Parto ~ predict.fit)

#tenemos los Pr(>|z|)   menores al 5% estos valores nos da indicios que este
#modelo tiene un buen ajuste a los datos

#Obtenemos los coeficientes del modelo
modelo_final$coefficients


#p= 1/(1+e^- (-8.6572539357+0.1919313013*Tiempo.de.Gestación+ 2.8777625245*Multiplicidad.de.EmbarazoSIMPLE
#             -0.0511914202*Edad.de.la.Madre+0.3121507046*Número.de.Hijos.Nacidos.Vivos-0.0001104267*Fecha.Anterior.del.Hijo.Nacido.Vivo)  


#Validacion cruzada 
library(caret)
set.seed(111)
control<- trainControl(method = "cv", number = 10) 
modelo <- train(
  Tipo.de.Parto ~ Tiempo.de.Gestación + Multiplicidad.de.Embarazo + 
  Edad.de.la.Madre + Número.de.Hijos.Nacidos.Vivos + Fecha.Anterior.del.Hijo.Nacido.Vivo, 
  data=datos_parto_modelo, # Datos de entrenamiento
  method = "glm",    # Método de modelado (en este caso, regresión lineal)
  family = binomial,
  trControl = control, # Control de validación cruzada
)
print(modelo)

