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


Antes <-  c(68,58,70,59,79,68,80,64,75,75,61,62)
Despues <-  c(80,65,80,70,88,77,90,75,87,82,70,74)
Diferencia <- Antes - Despues
length(Antes)
length(Despues)

#se analiza la normalidad de la variable Diferencia a partir de la densidad.
plot(density(Diferencia), main='Densidad para Diferencias', las=1, xlab='Diferencia de tiempo', ylab='Densidad')

# t de 
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
