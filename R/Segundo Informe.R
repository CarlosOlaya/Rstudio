################################################
#### PRUEBAS DE HIPÓTESIS PARA LA MEDIA ########
################################################

#Datos mayores que 30
#Hacer prueba de hipótesis para el promedio "a mano"
#Nota si tuviera los datos sueltos y fueran mas de 30
# debe calcular los valores dados abajo (Promedio, desviación y n)


# Ejemplo 1

xbarra <- 5.23  # Datos del problema
desvia <- 0.24   # Datos del problema
n <- 64        # Datos del problema
mu_0 <- 5.5      # Media de referencia

estad_prueba <- (xbarra - mu_0) / (desvia / sqrt(n))
estad_prueba  # Para obtener el valor del estadístico

Z<-qnorm(0.95)
Z # Para obtener el valor crít



#Con airquality plantear una hipotesis con cualquier variable de la data 
#La temperatura del año fue mayor de 80°
#Caso 2 muestras pequeñas

# Ejemplo 2

#ingresamos los datos
contenido <- c(510, 492, 494, 498, 492, 496, 502, 491, 507, 496) 
#Probamos la normalidad 
shapiro.test(contenido)
# el p-value debe ser mayor que el nivel de significancia alpha
#En nuestro caso p-value=0.1476>0.05=alpha
#se puede asumir que la muestra proviene de una población normal

#El ejercicio es una prueba bilateral
prueba_media<-t.test(contenido, alternative='two.sided', conf.level=0.95, mu=500)
prueba_media
#Usando la región crítica
prueba_media$statistic
#Valor crítico
qt(0.05/2,10-1)


##### Intervalos de confianza para la diferencia de medias con muestras no pareadas #########
##### Poblaciones independientes ########
#Ejemplo 7

install.packages("carData")
library(carData)
data("Salaries")
salario_hombres<-subset(Salaries,Salaries$sex=="Male")
head(salario_hombres)
class(salario_hombres)
salario_mujeres<-subset(Salaries,Salaries$sex=="Female")
head(salario_mujeres)
class(salario_mujeres)


#Caso 1: muestras grandes
dim(salario_hombres)
dim(salario_mujeres)

ic_indepen_diferencia_salarios1<- t.test(alternative="less", x=salario_hombres$salary, y=salario_mujeres$salary, paired=FALSE, conf.level=0.95)
ic_indepen_diferencia_salarios1
ic_indepen_diferencia_salarios1$conf.int

#Caso 2: muestras pequeñas con varianzas desconocidas y diferentes

ic_indepen_diferencia_salarios2<- t.test(x=salario_hombres$salary, y=salario_mujeres$salary, conf.level=0.95,var.equal = FALSE)
ic_indepen_diferencia_salarios2$conf.int


#Caso 3: muestras pequeñas con varianzas desconocidas e iguales

ic_indepen_diferencia_salarios3<- t.test(x=salario_hombres$salary, y=salario_mujeres$salary, conf.level=0.95,var.equal = TRUE)
ic_indepen_diferencia_salarios3$conf.int


################################################
#### INTERVALOS DE CONFIANZA PARA LA MEDIA #####
############### DOS POBLACIONES ################
################################################

##### Poblaciones dependientes-muestras pareadas ########
#Ejemplo 6
Antes   <- c(81, 87, 86, 82, 90, 86, 96, 73,74, 75, 72, 80, 66, 72, 56, 82)
Despues <- c(78, 91, 78, 78, 84, 67, 92, 70, 58, 62, 70, 58, 66, 60, 65, 73)
Diferencia <- Antes - Despues
 length(Antes)
 length(Despues)

#se analiza la normalidad de la variable Diferencia a partir de la densidad.
plot(density(Diferencia), main='Densidad para Diferencias', las=1, xlab='Diferencia de tiempo', ylab='Densidad')

# t de 
prueba_diferencia_medias<- t.test(x=Antes, y=Despues, paired=TRUE, conf.level=0.95)
prueba_diferencia_medias
prueba_diferencia_medias$conf.int


#################################################
###### PRUEBA DE HIPOTESIS PARA IGUALDAD DE VARIANZAS ####
#################################################

# Razon de varianzas
muestra1 <-  c(26.2,29.3,31.3,28.7,27.4,25.1,26,27.2,27.5,29.8,32.6,34.6)
muestra2 <-  c(25.3,28.2,29.2,27.1,26.8,26.5,30.7,31.3,26.3,24.2)

#Var. representa la prueba de varianza
prueba_var<-var.test( x <- muestra1,y <- muestra2,alternative='two.sided',conf.level=0.99)
prueba_var
prueba_var$p.value

##### PRUEBA DE NORMALIDAD ##########
set.seed(123)
n <- 100 
mu <- 10
sigma <- 1
x <- rnorm(n, mu, sigma)
hist(x)
par(mfrow=c(1,3))
histograma<-hist(x,freq = FALSE,main = "Histograma y densidad",
                 ylab = "Densidad")
densidad<-density(x)
lines(densidad,lwd=2,col="red")
plot(densidad, lwd = 2, col = "red",
     main = "Densidad")
rug(jitter(x))


qqnorm(x)
qqline(x,col="red",lwd=2)      
 
#Prueba estadistica para verificar la normalidad
shapiro.test(x)
#install.packages("nortest")
library(nortest)
lillie.test(x)

#Prueba de independencia  primero creacion de la tabla
tabla <- matrix(c(21,36,30,48,26,19), nrow=2, byrow=T)
tabla

colnames(tabla)<-c("No fumadores","Fum mod", "Fum Emper")
row.names(tabla)<-c("Con hipertension","Sin hipertension")
tabla
  # Parte 2 hallarel estadistico de prueba 
  pruebachisq<-chisq.test(tabla)
  estadistico_prueba<-pruebachisq$statistic
  estadistico_prueba
  #parte 3 
  alpha=0.05
  valor_critico<-qchisq(alpha,2,lower.tail = FALSE)
  valor_critico
  # Parte 4
  p_valor<-pruebachisq$p.value
  p_valor
  #Conclusiones 
  #criterio  1
  if (estadistico_prueba>valor_critico){
    print("Se rechaza H0")
  }else{
    print("No se rechaza H0")
  }
  #criterio 2
  if (p_valor<=alpha){
    print("Se rechaza H0")
  }else{
    print("No se rechaza H0")
  }
  #Conclusion con un nivel de significancia del 5% existe una relacion entre
  #la presencia o ausencia de Hiper y los habitos de fumar

library(FactoMineR)
acsimple<-CA(tabla)
acsimple$row$contrib
acsimple$col$contrib
help(CA)
acsimple_porcentajes_varianza<-CA(tabla)$eig
acsimple_porcentajes_varianza
library(ggplot2)
library(FactoClass)
library(factoextra)
#Perfiles fila
variables_fila=get_ca_row(acsimple)
variables_fila
#Puntos individuos fila
fviz_ca_row(acsimple, repel = TRUE)
#Perfiles columna
variables_columna=get_ca_col(acsimple)
#Puntos individuos columna
fviz_ca_col(acsimple, repel = TRUE)
#Contribuciones por fila
contribuciones_fila=variables_fila$contrib
contribuciones_fila
#Cosenos por fila
cosenos_fila=variables_fila$cos2
cosenos_fila
#contribuciones por columna
contribuciones_columna=variables_columna$contrib
contribuciones_columna
#Cosenos por columna
cosenos_columna=variables_columna$cos2
cosenos_columna



#Ejercicio de prueba de independencia y analisis de correspondencia
tabla1 <- matrix(c(20,20,20,40,10,40,20,10,40), nrow=3, byrow=T)
#tabla <- matrix(c(15, 11, 18, 13, 14, 15, 12, 18,14), nrow=3, byrow=T)

colnames(tabla)<-c("A","B","C","D")
row.names(tabla)<-c("Jovenes","Adultos","Tercera Edad")
tabla

# Parte 2 hallarel estadistico de prueba 
pruebachisq<-chisq.test(tabla)
estadistico_prueba<-pruebachisq$statistic
estadistico_prueba
#parte 3 
alpha=0.05
filas= nrow(tabla)
columnas=ncol(tabla)
grados_libertad = (filas-1) *( columnas-1)
valor_critico<-qchisq(alpha,grados_libertad,lower.tail = FALSE)
valor_critico
# Parte 4
p_valor<-pruebachisq$p.value
p_valor
#Conclusiones 
#criterio  1
if (estadistico_prueba>valor_critico){
  print("Se rechaza H0")
}else{
  print("No se rechaza H0")
}
#criterio 2
if (p_valor<=alpha){
  print("Se rechaza H0")
}else{
  print("No se rechaza H0")
}
#Conclusion con un nivel de significancia del 5% existe una relacion entre
#las ventas por productos y por categoria de edad 

library(FactoMineR)
acsimple<-CA(tabla)
acsimple$row$contrib
acsimple$col$contrib
help(CA)
acsimple_porcentajes_varianza<-CA(tabla)$eig
acsimple_porcentajes_varianza
library(ggplot2)
library(FactoClass)
library(factoextra)
#Perfiles fila
variables_fila=get_ca_row(acsimple)
variables_fila
#Puntos individuos fila
fviz_ca_row(acsimple, repel = TRUE)
#Perfiles columna
variables_columna=get_ca_col(acsimple)
#Puntos individuos columna
fviz_ca_col(acsimple, repel = TRUE)
#Contribuciones por fila
contribuciones_fila=variables_fila$contrib
contribuciones_fila
#Cosenos por fila
cosenos_fila=variables_fila$cos2
cosenos_fila
#contribuciones por columna
contribuciones_columna=variables_columna$contrib
contribuciones_columna
#Cosenos por columna
cosenos_columna=variables_columna$cos2
cosenos_columna



#Ejercicio de prueba de independencia
tabla <- matrix(c(113,34,21,32,117,31,25,27,130,40,20,10), nrow=3, byrow=T)
colnames(tabla)<-c("A","B", "C")
row.names(tabla)<-c("Jovenes","Adultos","Tercera Edad")
tabla

# Parte 2 hallar el estadistico de prueba 
pruebachisq<-chisq.test(tabla)
estadistico_prueba<-pruebachisq$statistic
estadistico_prueba
#parte 3 
alpha=0.05
filas= nrow(tabla)
columnas=ncol(tabla)
grados_libertad = (filas-1) *( columnas-1)
valor_critico<-qchisq(alpha,grados_libertad,lower.tail = FALSE)
valor_critico
# Parte 4
p_valor<-pruebachisq$p.value
p_valor
#Conclusiones 
#criterio  1
if (estadistico_prueba>valor_critico){
  print("Se rechaza H0")
}else{
  print("No se rechaza H0")
}
#criterio 2
if (p_valor<=alpha){
  print("Se rechaza H0")
}else{
  print("No se rechaza H0")
}
#Conclusion con un nivel de significancia del 5% existe una relacion entre
#las ventas por productos y por categoria de edad 





# Ejercio para AC
library("FactoMineR")
library("factoextra")

data(housetasks)
head(housetasks)
alpha=0.05
pruebachisq <- chisq.test(housetasks)
estadistico_prueba<-pruebachisq$statistic
estadistico_prueba

grados_libertad<-chisq$parameter

valor_critico<-qchisq(alpha,grados_libertad,lower.tail = FALSE)
valor_critico
#criterio  1
if (estadistico_prueba>valor_critico){
  print("Se rechaza H0")
}else{
  print("No se rechaza H0")
}

ac_simple<- CA(housetasks, graph = FALSE)
ac_simple

ac_simpleeig.val <- ac_simple$eig
eig.val

fviz_screeplot(ac_simple, addlabels = TRUE, ylim = c(0, 50))
fviz_screeplot(ac_simple) +
  geom_hline(yintercept=33.33, linetype=2, color="red")
fviz_ca_biplot(ac_simple, repel = TRUE)

row <- get_ca_row(ac_simple)
row

col <- get_ca_col(ac_simple)
col

row$cos2
col$cos2

#grafico de los cosenos para las filas  
fviz_ca_row(ac_simple,col.row = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))


# Ejercio para ACM
library("factoextra")
data(poison)
help("poison")
library(factoextra)
library(FactoMineR)
help("poison")

#Creamos subconjunto

poison.active<- poison [1:55,5:15]
head(poison.active [,1:6],3)

#Se realiza el análisis de correspondencia multiple.

res.mca <-MCA (poison.active, graph = FALSE)

#Gráfico de los porcentajes de varianza.

fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 45))

intercepcion <- 1 / (ncol(poison.active) - 1) * 100

fviz_screeplot(res.mca) +
  geom_hline(yintercept=intercepcion, linetype=2, color="red")

fviz_mca_biplot(res.mca,repel = TRUE ,ggtheme= theme_minimal())

var<- get_mca_var(res.mca)
var

head(var$coord)
head(var$cos2)
head(var$contrib)
fviz_mca_biplot(res.mca, choice= "mca.cor",repel = TRUE ,ggtheme= theme_minimal())


#Datos2
#CARGANDO PAQUETES
library(ggplot2)
library(corrplot)
library(dplyr)
library(readxl)
datos_pca_2 <- read_xlsx("C:/Users/tito_/Downloads/datos-pca-2.xlsx")

#LLAMAMOS LOS DATOS SOLO VAR NUMÉRICAS
datos2<-datos_pca_2[-1]
#Veamos las correlaciones entre variables
matriz_correl<-cor(datos2)
#grafico de correlaciones
corrplot( matriz_correl ,method = "number",type="lower")

#realizamos ACP
acp2<-prcomp(datos2,center = TRUE,scale=TRUE)
#Imprimimos resultados
acp2

#Summary del pca
summary(acp2)

#grafico de codo
plot(acp2,type="l")

#Reducimos dimensiones
acp2$rotation

#Componentes principales calculadas
comp1<-apply(acp2$rotation[,1]*datos2,1,sum)
comp2<-apply(acp2$rotation[,2]*datos2,1,sum)
comp1
comp2
#data set con las componentes
datos2$comp1<-comp1
datos2$comp2<-comp2
#Nuevo data set con acp
datos2[,1:5]<-NULL
#grafico de codo
library(factoextra)
fviz_screeplot(acp2, addlabels = TRUE, ylim = c(0, 100))



#Imputacion de datos 


install.packages(VIM)
install.packages(mice)
library(VIM)
library(mice)
#Conociendo mi data set
data("airquality")
help(airquality)
str(airquality)
summary(airquality)

#Contar el total de NAs en la base de datos
sum(is.na(airquality))

#Saber el número de NAs por columna
colSums(is.na(airquality))


#Omitir las filas con observaciones NA
air1<- na.omit(airquality)

#Contar el total de NAs en la base de datos
sum(is.na(air1))

#ver patrón en los valores perdidos
aggr(airquality,numbers=TRUE,ylab=c("Porcentaje de datos perdidos","Patrón"))

#Supongamos que la perdida de valores es aleatoria o completamente
#aleatoria porque...

#Vamos a hacer la imputación de los datos
#Imputar valores perdidos por la media
datos_imputados<- mice(airquality,method = "mean",seed = 123,print=F)
datos_completos<- mice::complete(datos_imputados)
head(datos_completos)
#Contar el total de NAs en la base de datos
sum(is.na(datos_completos))

#ver patrón en los valores perdidos
aggr(datos_completos,numbers=TRUE,ylab=c("Porcentaje de datos perdidos","Patrón"))

summary(datos_completos)

#Imputar valores perdidos por el metodo pmm Predictive mean matching
datos_imputados2<- mice(airquality,method = "pmm",seed = 123,print=F)

datos_completos2<- mice::complete(datos_imputados2)

sum(is.na(datos_completos2))
aggr(datos_completos2)

#Imputar valores perdidos por el metodo regresión
datos_imputados3<- mice(airquality,method = "norm.predict",seed = 123,print=F)

datos_completos3<- mice::complete(datos_imputados3)

sum(is.na(datos_completos3))



#Comparemos las estadísticas básicas
summary(airquality)
summary(datos_completos)
summary(datos_completos2)