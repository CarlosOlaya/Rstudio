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
#Conclusion con un nivel de significancia del 5% existe una relacion 


