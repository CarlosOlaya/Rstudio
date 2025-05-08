library(readxl)
library(ggplot2)
library(reshape2)

# Importar los datos desde el archivo Excel
datos_cluster <- read_excel("datos-cluster.xlsx")

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
datos_cluster <- read_excel("datos-cluster.xlsx")

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
