library(haven)
Datos_100_1_Caso <- read_sav("C:/Users/1075317251/OneDrive - cecar.edu.co/R/Datos_10_1_Caso.sav")
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



