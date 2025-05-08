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
  print("No se rechaza H0: se concluye Ua = Ud")
}
#Conclusion con un nivel de significancia del 5%
# se rechaza la hipotesis que el nuevo medicamento de tiene un tipo de cancer 



 
##Falta resforzar las pruebas de hipotesis de muestras que 30
##Falta reforzar cuando aplicar una varianza y como concluir la prueba de varianza v.test


