#### Informe 1 
#Conceptos Básicos
data("airquality")
#Que contiene ese dataframe
help("airquality")
#Como se lee cada variable y columna
help(str)
str(airquality)
#cambio te tipo de variable
#airquality$Day<-as.factor(airquality$Day)
str(airquality$Day)
airquality$Day<-as.numeric(airquality$Day)
airquality$Day<-as.integer(airquality$Day)

attach(airquality)

mean(Temp)
summary(airquality)
#solucion de calculo con valores NA
Ozone_sin_na<- mean(na.omit(Ozone))
# Imprimir el resultado
print(media_sin_na)

barplot(Temp, na.omit(Ozone), xlab = "Temperatura", ylab = "Ozono",
        main = "Relación entre Temperatura y Ozono", col = "purple")
hist(Temp, col = "red")

air_sin_na<-na.omit(airquality)
summary(air_sin_na)
hist(air_sin_na$Ozone, freq=FALSE, col="lightcyan" ,
     main="Histograma de ozone", xlab = "Ozono", ylab = "Densidad") #Histograma
lines(density(air_sin_na$Ozone), col="black",lwd=2)#Linea de densidad del histograma

# Histograma con densidad
ggplot(air_sin_na, aes(x = air_sin_na$Ozone)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "red") +
  geom_density()
library(ggplot2)
#Boxplot
ggplot(air_sin_na, aes(x =0 , y = air_sin_na$Ozone)) + 
  geom_boxplot()+
  geom_jitter()

#Incluyendo un factor
air_sin_na$Month<-as.factor(air_sin_na$Month)
ggplot(air_sin_na, aes(x =air_sin_na$Month , y = air_sin_na$Ozone, fill=air_sin_na$Month)) + 
  geom_boxplot()

ggplot(air_sin_na, aes(x =air_sin_na$Month , y = air_sin_na$Ozone, fill=air_sin_na$Month)) + 
  geom_boxplot()+
  guides(fill = guide_legend(title = "Meses"))
ggplot(air_sin_na, aes(x =air_sin_na$Month , y = air_sin_na$Ozone, fill=air_sin_na$Month)) + 
  geom_boxplot()+
  guides(fill = guide_legend(title = "Meses"))+
  scale_fill_hue(labels = c("Mes 1", "Mes 2", "Mes 3", "Mes 4", "Mes 5"))


####### TEMAS
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

colnames(tabla)<-c("A","B", "C")
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
head(housetasks)f
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

###Ejercicios Propuestos

#prueba de hipótesis de dos colas para determinar si el incremento porcentual
#promedio de las utilidades de todas  las empresas licoreras fue diferente a 14%.

#ingresamos los datos
contenido <- c(16.1,14.4,12.9,13.7,14.9,14.6,12.5,15.4) 
contenido

#Probamos la normalidad 
shapiro.test(contenido)
nivel_significancia<-0.05 
p_value_sp<- shapiro.test(contenido)$p.value
p_value

# el p-value debe ser mayor que el nivel de significancia alpha
if (p_value_sp>nivel_significancia){
  print("se puede asumir que la muestra proviene de una población normal")
}else {
  print("La muestra no tiene una distribución normal")
}

#Se hace la prueba dependiendo el tipo de hipotesis en este caso es de dos colas
prueba_media<-t.test(contenido, alternative='two.sided', conf.level = 0.95, mu=14)
prueba_media
grados_libertad<- prueba_media$parameter
grados_libertad

#Usando la región crítica
estadistico_prueba<-prueba_media$statistic
estadistico_prueba

#Valor crítico entre dos porque es de dos colas
valor_t=qt(nivel_significancia/2,grados_libertad)
valor_t

# Criterio 1 Comparando con el valor crítico de t
if (abs(estadistico_prueba) >abs( valor_t)) {
  print("Se rechaza H0")
}else{
  print("No se rechaza H0")
}
#Conclusion con un nivel de significancia del 5% se rechaza la hipotesis 
#de que el promedio porcentual  fue diferente del 14%






#prueba de hipótesis del contenido promedio de las bolsas de detergente es de 
#10 kilogramos, utilizando una muestra aleatoria de 10 bolsas

#ingresamos los datos
contenido <- c(10.2, 9.7, 10.1, 10.3 , 10.1, 9.8, 9.9, 10.4, 10.3, 9.8) 
contenido

#Probamos la normalidad 
shapiro.test(contenido)
nivel_significancia<-0.01
p_value_sp<- shapiro.test(contenido)$p.value
p_value_sp

# el p-value debe ser mayor que el nivel de significancia alpha
if (p_value_sp>nivel_significancia){
  print("se puede asumir que la muestra proviene de una población normal")
}else {
  print("La muestra no tiene una distribución normal")
}

#Se hace la prueba dependiendo el tipo de hipotesis en este caso es de dos colas
prueba_media<-t.test(contenido, alternative='two.sided',conf.level = 0.99, mu=10)
prueba_media
grados_libertad<- prueba_media$parameter
grados_libertad

#Usando la región crítica
estadistico_prueba<-prueba_media$statistic
estadistico_prueba

#Valor crítico entre dos porque es de dos colas
valor_t=qt(nivel_significancia / 2 , grados_libertad)
valor_t

# Criterio 1 Comparando con el valor crítico de t 
if (abs(estadistico_prueba) > abs(valor_t)) {
  print("Se rechaza H0")
} else {
  print("No se rechaza H0")
}

#Conclusion con un nivel de significancia del 1%
#no hay suficiente evidencia para rechazar la hipótesis que el contenido es de 10kg



#Encontrar si un nuevo medicamento detiene un tipo de cancer con una muestra de 5 y 4
# Poblaciones dependientes-muestras pareadas
con_tratamiento  <- c(1.4, 4.6, 0.9, 2.1, 5.3)
sin_tratamiento  <- c(2.8, 3.1, 1.9, 0.5)

nivel_significancia<-0.05

prueba_diferencia_var_iguales<- t.test(x=con_tratamiento, y=sin_tratamiento, conf.level=0.95,var.equal = TRUE)
prueba_diferencia_var_iguales
prueba_diferencia_var_iguales$conf.int

p_value = prueba_diferencia_var_iguales$p.value
p_value

if (p_value < nivel_significancia ) {
  print("Se rechaza H0: se concluye Ua != Ud")
} else {
  print("No se rechaza H0: se concluye  h1 Ua = Ud")
}
#Conclusion con un nivel de significancia del 5%
# se rechaza la hipotesis que el nuevo medicamento de tiene un tipo de cancer 

### quiz 1
# Se considera 3 configuraciones diferentes de diseño para un componente particular
#Hay cuatro posibles modos de falla para el componente
#¿La configuracion parece tener efecto sobre el tipo de falla?

#Parte 1 creacion de la mtriz de los datos del problema planteado
tabla <- matrix(c(20, 44, 17, 9, 4, 17, 7, 12, 10, 31, 14, 5),nrow=3, byrow=T)
colnames(tabla)<-c("1","2","3","4")
row.names(tabla)<-c("1","2","3")
tabla

# Parte 2 hallar el estadistico de prueba 
pruebachisq<-chisq.test(tabla)
pruebachisq
estadistico_prueba<-pruebachisq$statistic
estadistico_prueba
#parte 3 
alpha=0.05
filas= nrow(tabla)
columnas=ncol(tabla)
grados_libertad
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
#Conclusion con un nivel de significancia del 5% existe una relacion se rechaza h0... 
# no se rechaza h1 por ende podemos concluir que....


### Ejercico en clase 
#Tomamos el data set a manejar
data("rivers")
#Usamos la funcion help para explotar el data set
help(rivers)
#Hacemos un summary para  interpretar
summary(rivers)

#Ploteo de caja para observar la ubicacion de los datos 
boxplot(rivers)
boxplot(rivers)$out
length(boxplot(rivers)$out)

shapiro.test(rivers)
par(mfrow=c(1,2))
hist(rivers,breaks= 30)
qqnorm(rivers)
qqline(rivers)

myrivers <- data.frame(rivers)
myrivers
myoutliers <- boxplot(rivers)$out
myoutliers
boxplot(myoutliers)

# Obtener los índices de los valores atípicos en el dataframe original
indices_outliers <- which(myrivers %in% myoutliers)
indices_outliers

#prueba
Antes <-  c(68,58,70,59,79,68,80,64,75,75,61,62)
Despues <-  c(80,65,80,70,88,77,90,75,87,82,70,74)
Diferencia <- Antes - Despues
length(Antes)
length(Despues)

#se analiza la normalidad de la variable Diferencia a partir de la densidad.
plot(density(Diferencia), main='Densidad para Diferencias', las=1, xlab='Diferencia de tiempo', ylab='Densidad')

# t test
prueba_diferencia_medias <- t.test(x=Antes, y=Despues, paired=TRUE, conf.level=0.95)
prueba_diferencia_medias
p_value <- prueba_diferencia_medias$p.value
conf_int <- prueba_diferencia_medias$conf.int
H0 <- "Ui = Ud,  el promedio de las muestras son iguales"
H1 <- "Ui != Ud, el promedio de las muestras son diferentes"

if (p_value < 0.005) {
  print(paste("Se rechaza y se concluye", H0))
} else {
  print("No se rechaza y se concluye H0", H1)
}
print("Como se rechaza H0, concluimos que las pulsaciones se ven afectadas por el consumo de bebidas")
prueba_diferencia_medias$estimate
print("Como el promedio de diferencia es menor que 0 nos muesta que las pulsaciones aumentan despues de beber ")


Antes <-  c(1.4,4.6,0.9,2.1,5.3)
Despues <-  c(2.8,3.1,1.9,0.5)
Diferencia <- Antes - Despues
length(Antes)
length(Despues)

#se analiza la normalidad de la variable Diferencia a partir de la densidad.
plot(density(Diferencia), main='Densidad para Diferencias', las=1, xlab='Diferencia de tiempo', ylab='Densidad')


###PRUEBAS DE HOMOGENEIDAD###

#Ingresamos tabla de contingencia#

tabla<-matrix(c(80,40,20,100,45,25,120,30,10),nrow = 3,byrow=TRUE)
tabla

#Datos de tabulación que detallan el tipo de hormona vs los efectos.

colnames(tabla)<-c("Sin Efectos","Efectos Leves","Efectos Graves")
row.names(tabla)<-c("Tipo I","Tipo II","Tipo III")
tabla

#Hallamos el estadístico de preba
chisq.test(tabla)
test<-chisq.test(tabla)
test$statistic

#Valor crítico
qchisq(0.05,4,lower.tail = FALSE)

library(FactoMineR)
grafico <- CA(tabla)
grafico$col$contrib
grafico$row$contrib

#Hipotesis
#H0->No Hay Diferencia
#H1->Hay Diferencia


#H0->Los tratamientos hormonales son homogéneos con respecto al tipo de efecto.
#H1->Los tratamientos hormonales no son homogéneos con respecto al tipo de efecto.

#Criterio1: Como el estadístico de prueba (14.33) es mayor que el valor crítico (9.487) se rechaza H0.

#Criterio2: Como el p-value (0.006) es menor que el alpha = 0.05, se rechaza H0.

#Conclusión: Los tratamientos hormonales no son homogéneos con respecto al tipo de efecto
