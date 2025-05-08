library(carData)
data("Salaries")
# variables a tomar rango y salario para determinar si el salario depende del rango
rango<-Salaries$rank
salario<-Salaries$salary
rango
salario 
summary(salario)

# Definir los límites de los rangos para no manejar tantos datos
limites <- c(-Inf, 91000, 134185, Inf)
nombres <- c("Menos de 91000", "De 91000 a 134185", "Más de 134185")
# Crear una nueva variable con los rangos
salario_new <- cut(Salaries$salary, breaks = limites, labels = nombres, right = FALSE)


# Prueba de independencia
#creacion de la tabla con salario en rangos
tabla<-table(salario_new,rango)
tabla

# Parte 2 hallar el estadistico de prueba 
pruebachisq<-chisq.test(tabla)
estadistico_prueba<-pruebachisq$statistic
estadistico_prueba
#parte 3 hallar el valor critico
alpha=0.05
valor_critico<-qchisq(alpha,2,lower.tail = FALSE)
valor_critico
# Parte 4 hallar el  p valor
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
#Conclusion con un nivel de significancia del 5% existe una relacion entre salario y rango

#Prueba de analisis de correspondencia AC

library(FactoMineR)
acsimple<-CA(tabla)
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
variables_columna
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
fviz_ca_row(acsimple,col.row = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
 



