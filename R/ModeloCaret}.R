data("mtcars")
modelo1<-lm(mpg ~ . , data=mtcars)
sumarry(modelo1)
library(MASS)
backward_step<- stepAIC(modelo1,direction = "backward")
modelo_final<-lm(mpg ~ wt + qsec + am, data=mtcars)
summary(modelo_final)

#Validación cruzada
install.packages("caret")
library(caret)
set.seed(300)
control<- trainControl(method = "cv", number = 10) # 5-fold cross-validation
modelo <- train(
  mpg ~ wt + qsec + am,# Fórmula del modelo
  data =mtcars,    # Datos de entrenamiento
  method = "lm",    # Método de modelado (en este caso, regresión lineal)
  trControl = control # Control de validación cruzada
)
print(modelo)
