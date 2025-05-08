#Iniciamos un analisis exploratorio de los datos Pima Indians Diabetes Database
#Llamamos los datos del archivo Excel
datos_diabetes <- read.csv("C:/Users/tito_/OneDrive - cecar.edu.co/R/ProyectoR/diabetes.csv")

#Renombramos las variables o columnas para un mejor entendimiento del dataset
colnames(datos_diabetes)<- c("Embarazos","Glucosa","PresionArterial","GrosorPiel","Insulina","IMC","DiabetesPedigrinFuncion","Edad","Resultado")
#consultamos sus datos y tipos de datos
str(datos_diabetes)
#Hacemos un analisis estadistico
summary(datos_diabetes)
#Al observar los resultados notamos que algunas vairables tienen un valor minimo de 0
#lo que nos indica que existen valores 0 en el dataset, procedemos a analizar 
#si son relevantes o no.
#Algunos datos son necesarios de trabajar ya que en la vida real no existe un nivel
#de glucosa, presión arterial, grosor de piel,imc o nivel de insulina en 0 por ende
#procedemos a convertir los 0 en NA
datos_diabetes$Glucosa[datos_diabetes$Glucosa == 0] <- NA
datos_diabetes$PresionArterial[datos_diabetes$PresionArterial == 0] <- NA
datos_diabetes$GrosorPiel[datos_diabetes$GrosorPiel == 0] <- NA
datos_diabetes$Insulina[datos_diabetes$Insulina == 0] <- NA
datos_diabetes$IMC[datos_diabetes$IMC == 0] <- NA

#omitimos la columnas embarazos ya que existen mujeres que aun no estan embarazadas
# asi como personas con resultado de diabetes en 1 o 0

#solucion de calculo con valores NA
datos_diabetes_sinNA<- na.omit(datos_diabetes)
# verificamos 
datos_diabetes_sinNA
summary(datos_diabetes_sinNA)

#convertimos en factor nuestra variable objetivo
datos_diabetes_sinNA$Resultado <- as.factor(datos_diabetes_sinNA$Resultado)
levels(datos_diabetes_sinNA$Resultado)

#Inspeccionamos los datos nuevamente
str(datos_diabetes_sinNA)
head(datos_diabetes_sinNA)


#Miramos la distribuciuon de los datos en cada una de las variables
library(ggplot2)
library(gridExtra)
#Grafico de cajas 
boxplots <- list(
 ggplot(datos_diabetes_sinNA, aes(x = 0, y = Embarazos)) + geom_boxplot() + geom_jitter(),
 ggplot(datos_diabetes_sinNA, aes(x = 0, y = Glucosa)) + geom_boxplot() + geom_jitter(),
 ggplot(datos_diabetes_sinNA, aes(x = 0, y = PresionArterial)) + geom_boxplot() + geom_jitter(),
 ggplot(datos_diabetes_sinNA, aes(x = 0, y = GrosorPiel)) + geom_boxplot() + geom_jitter(),
 ggplot(datos_diabetes_sinNA, aes(x = 0, y = Insulina)) + geom_boxplot() + geom_jitter(),
 ggplot(datos_diabetes_sinNA, aes(x = 0, y = IMC)) + geom_boxplot() + geom_jitter(),
 ggplot(datos_diabetes_sinNA, aes(x = 0, y = DiabetesPedigrinFuncion)) + geom_boxplot() + geom_jitter(),
 ggplot(datos_diabetes_sinNA, aes(x = 0, y = Edad)) + geom_boxplot() + geom_jitter(),
 ggplot(datos_diabetes_sinNA, aes(x = 0, y = Resultado)) + geom_boxplot() + geom_jitter())
grid.arrange(grobs = boxplots, ncol = 3)

#Grafico de histograma

par(mfrow = c(3, 3), mar = c(4, 4, 2, 1))
hist(datos_diabetes_sinNA$Embarazos, main = "Histograma Embarazo", ylab = "Densidad")
hist(datos_diabetes_sinNA$Glucosa, main = "Histograma Glucosa", ylab = "Densidad")
hist(datos_diabetes_sinNA$PresionArterial, main = "Histograma PresionArterial", ylab = "Densidad")
hist(datos_diabetes_sinNA$GrosorPiel, main = "Histograma GrosorPiel", ylab = "Densidad")
hist(datos_diabetes_sinNA$Insulina, main = "Histograma Insulina", ylab = "Densidad")
hist(datos_diabetes_sinNA$IMC, main = "Histograma IMC", ylab = "Densidad")
hist(datos_diabetes_sinNA$DiabetesPedigrinFuncion, main = "Histograma DiabetesPedigrinFuncion", ylab = "Densidad")
hist(datos_diabetes_sinNA$Edad, main = "Histograma Edad", ylab = "Densidad")

#Creacion de la regresion Logistica
#Separamos los datos de entrenamiento y de validación
# Crear un vector de índices aleatorios
set.seed(123)  # Para reproducibilidad
indices <- sample(nrow(datos_diabetes_sinNA))

# Calcular el número de filas para entrenamiento 80% y prueba 20%
n_train <- round(0.8 * nrow(datos_diabetes_sinNA))
n_test <- nrow(datos_diabetes_sinNA) - n_train

# Dividir los datos en conjuntos de entrenamiento y prueba
train_indices <- indices[1:n_train]
test_indices <- indices[(n_train + 1):nrow(datos_diabetes_sinNA)]

# Crear conjuntos de entrenamiento y prueba
datos_diabetes_train <- datos_diabetes_sinNA[train_indices, ]
datos_diabetes_test <- datos_diabetes_sinNA[test_indices, ]




