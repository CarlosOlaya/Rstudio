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
  library(ggplot2)

# Histograma con densidad
ggplot(air_sin_na, aes(x = air_sin_na$Ozone)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "red") +
  geom_density()
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

