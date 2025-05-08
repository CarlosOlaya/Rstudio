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
