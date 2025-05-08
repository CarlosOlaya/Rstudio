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





library(haven)
Datos_100_1_Caso <- read_sav("C:/Users/tito_/OneDrive - cecar.edu.co/R/Datos_10_1_Caso.sav")
datos <- Datos_100_1_Caso[,1:4]
datos <- na.omit(datos)
str(datos)

attach(datos)
modelo_simulado<-glm(survived~pclass+sex+age,family = "binomial")
summary(modelo_simulado)

#Modelo nulo no hay variable expli
#Null deviance: 1414.62  on 1045  degrees of freedom
#su gl=n-1 numero de observaciones
#modelo completo incorporta todas las variables
#Residual deviance:  983.02  on 1042  degrees of freedom
#su gl=n-4 menos el numero del parametros 
#AIC: 991.02
#Verificacion de la significancia del modelo
deviance.residual<-modelo_simulado$deviance
deviance.nulo<-modelo_simulado$null.deviance
estadistico_chi2<-deviance.nulo-deviance.residual
grados_libertad_chi<-modelo_simulado$df.null - modelo_simulado$df.residual
p_valor<-1-pchisq(estadistico_chi2,df=grados_libertad_chi)
p_valor

#verificar la significancia de cada variable 
summary(modelo_simulado)

#Evaluando el ajuste del modelo
#Pseudo R^2 de McFadden
R2MF<-(deviance.nulo-deviance.residual)/deviance.nulo
R2MF*100

#R^2 de Cox y Snell
N<-dim(datos)[1]
R2CS<-1-exp(1/N*(deviance.residual-deviance.nulo))
#R^2 de Nagelkerke
R2N<-R2CS/(1-exp(-deviance.nulo/N))
R2N*100

#Matriz de confusión
library(gmodels)
predict.fit<-modelo_simulado$fitted.values
set.seed(123)
predict.fit[predict.fit>=0.5]<-1
predict.fit[predict.fit<0.5]<-0
matriz<-CrossTable(datos$survived,predict.fit,prop.chisq = FALSE,prop.r = F,prop.c = F)

matriz$t
matriz$prop.tbl
#especificidad
matriz$prop.row[1,1]
#sensibilidad
matriz$prop.row[2,2]

#Curvas ROC
library(Epi)
ROC(data = datos, form = survived~sex+pclass+age)
ROC(data = datos, form = survived~sex)
ROC(data = datos, form = survived~pclass)
ROC(data = datos, form = survived~age)



library(readxl)
library(ggplot2)
library(reshape2)

# Importar los datos desde el archivo Excel
datos_cluster <- read_excel("C:/Users/tito_/OneDrive - cecar.edu.co/R/datos-cluster.xlsx")

# Ver la estructura de los datos
str(datos_cluster)

# Gráfico de dispersión por ser dos variables
plot(datos_cluster$INVERSION, datos_cluster$VENTAS, pch = 19)

# Asignamos las etiquetas
text(datos_cluster$INVERSION, datos_cluster$VENTAS,
     labels = row.names(datos_cluster),
     cex = 0.5, pos = 4, col = "red")

# Matriz de distancias
# Distancia Euclidea
matriz_dist_euclidia <- dist(datos_cluster[-1], method = "euclidean", diag = TRUE)
round(matriz_dist_euclidia, 2)

cluster_centroide<-hclust(matriz_dist_euclidia,method="centroid")

# Distancia Manhattan
matriz_dist_manhattan <- dist(datos_cluster[-1], method = "manhattan", diag = TRUE)
round(matriz_dist_manhattan, 2)

# Distancia Minkowski
matriz_dist_minkowski <- dist(datos_cluster[-1], method = "minkowski", diag = TRUE)
round(matriz_dist_minkowski, 2)

# Escalamiento de los datos excluyendo la columna 'Empresa'
datos_escalados <- scale(datos_cluster[-1])

# Matriz de distancias escaladas
# Distancia Euclidea después de escalar
matriz_dist_euclidia_escalada <- dist(datos_escalados, method = "euclidean", diag = TRUE)
round(matriz_dist_euclidia_escalada, 2)

# Distancia Manhattan después de escalar
matriz_dist_manhattan_escalada <- dist(datos_escalados, method = "manhattan", diag = TRUE)
round(matriz_dist_manhattan_escalada, 2)

# Distancia Minkowski después de escalar
matriz_dist_minkowski_escalada <- dist(datos_escalados, method = "minkowski", diag = TRUE)
round(matriz_dist_minkowski_escalada, 2)

# Graficar las matrices de distancia antes y después del escalamiento
# Convertir las matrices de distancias en dataframes para ggplot
df_dist_euclidia_antes <- as.data.frame(as.table(as.matrix(matriz_dist_euclidia)))
df_dist_euclidia_despues <- as.data.frame(as.table(as.matrix(matriz_dist_euclidia_escalada)))

# Graficar las distancias euclidianas antes del escalamiento
ggplot(data = df_dist_euclidia_antes, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  ggtitle("Distancias Euclidianas Antes del Escalamiento") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Graficar las distancias euclidianas después del escalamiento
ggplot(data = df_dist_euclidia_despues, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  ggtitle("Distancias Euclidianas Después del Escalamiento") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


--------------------------------------------------------------------------------
  
  
library(readxl)

# Importar los datos desde el archivo Excel
datos_cluster <- read_excel("C:/Users/tito_/OneDrive - cecar.edu.co/R/datos-cluster.xlsx")

# Ver la estructura de los datos
str(datos_cluster)

# Gráfico de dispersión por ser dos variables
plot(datos_cluster$INVERSION, datos_cluster$VENTAS, pch = 19, main = "Gráfico de Dispersión", xlab = "INVERSION", ylab = "VENTAS")

# Asignamos las etiquetas
text(datos_cluster$INVERSION, datos_cluster$VENTAS,
     labels = row.names(datos_cluster),
     cex = 0.5, pos = 4, col = "red")

# Matriz de distancias
# Distancia Euclidea
matriz_dist_euclidia <- dist(datos_cluster[-1], method = "euclidean", diag = TRUE)
round(matriz_dist_euclidia, 2)

# Distancia Manhattan
matriz_dist_manhattan <- dist(datos_cluster[-1], method = "manhattan", diag = TRUE)
round(matriz_dist_manhattan, 2)

# Distancia Minkowski
matriz_dist_minkowski <- dist(datos_cluster[-1], method = "minkowski", diag = TRUE)
round(matriz_dist_minkowski, 2)

# Escalamiento de los datos excluyendo la columna 'Empresa'
datos_escalados <- scale(datos_cluster[-1])

# Matriz de distancias escaladas
# Distancia Euclidea después de escalar
matriz_dist_euclidia_escalada <- dist(datos_escalados, method = "euclidean", diag = TRUE)
round(matriz_dist_euclidia_escalada, 2)

# Distancia Manhattan después de escalar
matriz_dist_manhattan_escalada <- dist(datos_escalados, method = "manhattan", diag = TRUE)
round(matriz_dist_manhattan_escalada, 2)

# Distancia Minkowski después de escalar
matriz_dist_minkowski_escalada <- dist(datos_escalados, method = "minkowski", diag = TRUE)
round(matriz_dist_minkowski_escalada, 2)

# Análisis de clúster con método del centroide
cluster_centroide <- hclust(matriz_dist_euclidia, method = "centroid")

# Dendrograma antes del escalamiento
plot(cluster_centroide, main = "Dendrograma Antes del Escalamiento", xlab = "", sub = "", cex = 0.6)

# Análisis de clúster con método del centroide después del escalamiento
cluster_centroide_escalado <- hclust(matriz_dist_euclidia_escalada, method = "centroid")

# Dendrograma después del escalamiento
plot(cluster_centroide_escalado, main = "Dendrograma Después del Escalamiento", xlab = "", sub = "", cex = 0.6)

# Obtención del historial de aglomeración
cluster_centroide_resumen <- data.frame(cluster_centroide[2:1])
cluster_centroide_resumen


#Instalacion de paquetes si es necesario
#install.packages("ggplot2")
#install.packages("cluster")
#install.packages("NbClust")
#install.packages("dendextend")
#install.packages("factoextra")
#install.packages("dplyr")

# librerias
library(cluster)
library(ggplot2)
library(NbClust)
library(dendextend)
library(factoextra)
library(dplyr)

# Cargamos el conjunto de datos
data(mtcars)
str(mtcars)

# Como algunos tienen valores muy grandes a comparacion de otras vataibles
# Se hace un escalado y almacenamos los nuevos datos escalados
datos_escalados <- scale(mtcars)

# Calculamos la matriz de distancias con los datos_escalados
distancias <- dist(datos_escalados, method = "euclidean")

# Realizar el clustering jerárquico meidante el método de Ward con la mastriz calculada
hc1 <- hclust(distancias, method = "ward.D")

# Graficar el dendrograma
plot(hc1, cex = 0.6, hang = -1, main = "Dendrograma usando el método de Ward")

# Determinar el número óptimo de clusters usando NbClust
set.seed(123) # Asegurar reproducibilidad
res <- NbClust(datos_escalados, distance = "euclidean", min.nc = 2, max.nc = 10, method = "ward.D", index = "silhouette")

# Extraer el número óptimo de clusters sugerido por la mayoría de índices
optimal_clusters <- res$Best.nc["Number_clusters"]
print(paste("Número óptimo de clusters sugerido por la mayoría de índices:", optimal_clusters))

# Graficar el dendrograma marcando los clousters optimos
plot(hc1, cex = 0.6, hang = -1, main = "Dendrograma con corte óptimo")
rect.hclust(hc1, k= optimal_clusters , border = "red")

# Cortar el dendrograma  y obtenemos el resultado con el número óptimo de clusters
clust <- cutree(hc1, k = optimal_clusters)

#Podemos Observar que observacion pertenece al cluster#
resultado_cluster <- data.frame(datos_escalados, clust = factor(clust))

# Visualizar los clusters en una nueva columna de forma grafica
fviz_cluster(list(data = datos_escalados, cluster = clust))

# Obtener el historial de aglomeración
cluster_resumen <- data.frame(hc1$merge, height = hc1$height)
names(cluster_resumen) <- c("Cluster1", "Cluster2", "Altura")
print(cluster_resumen)

# Agrupar por clust y resumir las medias de las variables deseadas
cluster_mtcars <- resultado_cluster %>% group_by(clust) %>%
  summarise(mean(mpg),mean(cyl),mean(disp),mean(hp),mean(drat),
            mean(wt),mean(qsec),mean(vs),mean(am),mean(gear),
            mean(carb))
cluster_mtcars





#install.packages("ggplot2")
library(cluster)
library(ggplot2)
library(carData)
data("Angell")
str(Angell)


datos<-Angell[-4]
datos<- na.omit(datos)
datos<- scale(datos)
d <- dist(datos, method = "euclidean")
hc1<- hclust(d, method = "ward.D" )
dendograma<-plot(hc1, cex = 0.6, hang = -1)
#Usamos agnes para 
#Medir la cantidad de estructura de agrupamiento encontrada
#los valores más cercanos a 1 sugieren una estructura de agrupación fuerte
hc2<-agnes(d,method = "ward")
coef_aglomeración<-hc2$ac
coef_aglomeración

library(NbClust)
#La regla de la mayoría.
res<-NbClust(datos,distance = "euclidean",min.nc = 4,
             max.nc = 8,method = "ward.D",index = "alllong")

#install.packages("dendextend")
library(dendextend)
clust<-cutree(hc1, k = 5)

library(factoextra)
fviz_cluster(list(data = datos, cluster = clust))

#Obtener el historial de aglomeración
cluster_resumen<-data.frame(hc1[2:1])
cluster_resumen


par(mfrow=c(1,2))
dendograma<-plot(hc1, cex = 0.6, hang = -1)
rect.hclust(hc1, k = 4, border = "red")
dendograma<-plot(hc1, cex = 0.6, hang = -1)
rect.hclust(hc1, h = 7, border = "red")


#También podemos dar color a las ramas de los grupos
dend <- color_branches(hc1, k = 4)
plot(dend)
ggplot(dend)

#No jerarquico
#Kmeans
kmedias<-kmeans(datos,4)
kmedias
#obtenemos las medias
aggregate(datos,by=list(kmedias$cluster),FUN=mean)
#adicionamos la pertencia al cluster
datos_cluster_kmedias<-data.frame(datos,kmedias$cluster)
datos_cluster_kmedias
fviz_cluster (kmedias, data = datos)

