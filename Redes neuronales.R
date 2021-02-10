#Proyecto ENIGH

#Leamos la base de datos
datos<-read.csv("~/Documents/Data Mining/concentradohogaror.csv", header = T,sep = ",")

#Quitemos las variables que no gastan
noc<-which(datos$gasto_mon==0)
datos<-datos[-noc,]

#Nos quedamos con las variables que nos interesan y creemos la variable respuesta
attach(datos)

datos[,c(77,80,81,89,96,100,103,104)] <-datos[,c(77,80,81,89,96,100,103,104)]/gasto_mon
output<-ing_cor/gasto_mon
output<-ifelse(output<1,1,0)#1 gasta,0 ahorran 

base1<-data.frame(output, datos[,c(6,11,12,13,77,80,81,89,96,100,103,104)])

base1$sexo_jefe<-as.numeric(base1$sexo_jefe)
base1$educa_jefe <-as.numeric(base1$educa_jefe)


#Hagamos una muestra para hacer las predicciones con el 50% de la base original
index <- sample(1:nrow(base1),round(0.50*nrow(base1)))
#Guardamoas nuestra base completa en una nueva varibale y tomamos la muestra
base0<-base1
base1<-base1[index,]
#Hagamos las muestras de entrenamiento con el 70% de nuestra muentra
index <- sample(1:nrow(base1),round(0.70*nrow(base1)))
train <- base1[index,]
test <- base1[-index,]

#Ajustamos la red neuronal con la muestra de entrenamiento 
library(neuralnet)
n <- names(train)
f <- as.formula(paste("output ~", paste(n[!n %in% "output"], collapse = " + ")))
set.seed(500)

nn3 <- neuralnet(f,data=train,hidden=6,linear.output = F,threshold=0.5, err.fct = "ce")
plot(nn3)
#Veamos las tasas de error con las distintas muestras que tenemos
##Tasa de error para train
pred.nn <- compute(nn3,train[,2:13])
tabla=cbind(train,ifelse(pred.nn$net.result>.5,1,0))
names(tabla)[14]<-"prediccion"
table(tabla$output,tabla$prediccion)
a<-prop.table(table(tabla$output,tabla$prediccion))
a[1,2]+a[2,1]

##Tasa de error para test
pred.nn <- compute(nn3,test[,2:13])
tabla=cbind(test,ifelse(pred.nn$net.result>.5,1,0))
names(tabla)[14]<-"prediccion"
b<-table(tabla$output,tabla$prediccion)
a<-prop.table(b)
a[1,2]+a[2,1]

#Resultados
clean<-data.frame(prediccion=pred.nn$net.result,output=test$output)

#########################Cross validation
CVV<-c()
library(plyr) 
pbar <- create_progress_bar('text')
k<-10
pbar$init(k)

for(i in 1:k){
  idx <- sample(1:nrow(base1),round(0.60*nrow(base1)))
  train_cv <- base1[idx,]
  test_cv <- base1[-idx,]
  nn1 <- neuralnet(f,data=train_cv,hidden=6,linear.output = F,threshold=0.6, err.fct = "ce")
  pred.nn1 <- compute(nn1,test_cv[,2:13])
  tabla=cbind(test_cv,ifelse(pred.nn1$net.result>.5,1,0))
  names(tabla)[14]<-"prediccion"
  table(tabla$output,tabla$prediccion)
  a<-prop.table(table(tabla$output,tabla$prediccion))
  CVV<-c(CVV,a[1,2]+a[2,1])
  pbar$step()
}

#Mostremos los resultados del cross validation
CVV_w10<-c(CVV_w7,CVV)
mean(CVV_w10)

#Hagamos la tabla ROC y nos basaremos en ella para ver cual es la mejor prediccion
library("ROSE")
pred.nn <- compute(nn3,base0[,2:13])
roc.curve(base0$output,pred.nn$net.result) #0.712



#Dejemos guardada una red neuronal con 6 neuronas
nn_6n

"
9 neuronas Error promedio: 0.18314 y ROC:0.712
4 neuronas Error promedio: 0.1807688889 y ROC: 0.716
7 no convergia
6 neuronas Error promedio: 0.1796377778 y ROC: 0.706
8 no converge
3 neuronas Error promedio: 0.1814444444 y ROC:0.705
5 neuronas Error promedio:0.1818955556 y ROC: 0.716
10 no converge
"

#######Análisis descriptivo######
names(base0)
#output<-ing_cor/gasto_mon
#1 gasta,0 ahorran
detach(datos)
attach(base0)

gast<-subset(base0, subset = output==1)
ahrr<-subset(base0, subset = output==0)


#Analicemos cada una de las variables
#Estrato socioeconómico
es<-prop.table(table( output, est_socio), margin = 1)
dimnames(es)$est_socio<-c("Bajo", "Medio bajo", "Medio alto", "Alto")
dimnames(es)$output<-c("Ahorran", "Se endeudan")
barplot(es,beside = T, legend.text = T, col=c("deeppink4","darkturquoise"), main="Estrato Socioecómico",ylab = "Frecuencia absoluta")


#Sexo
sex<-prop.table(table(output,sexo_jefe), margin = 1)
dimnames(sex)$sexo_jefe<-c("Hombre", "Mujer")
dimnames(sex)$output<-c("Ahorran", "Se endeudan")
barplot(sex,beside = T, legend.text = T, col=c("deeppink4","darkturquoise"), main="Sexo",ylab = "Frecuencia absoluta")

#Edad
ed<-prop.table(table(output,edad_jefe), margin = 1)
dimnames(ed)$output<-c("Ahorran", "Se endeudan")
barplot(ed,beside = T,col=c("deeppink4","darkturquoise"), main="Edad", legend.text = T,ylab = "Frecuencia absoluta",space = c(0.2,0.2))

#Educación
edu<-prop.table(table(output,educa_jefe), margin = 1)
dimnames(edu)$output<-c("Ahorran", "Se endeudan")
dimnames(edu)$educa_jefe<-c("Sin inst","Pre","Prim inc","Prim","Secu inc","Secu","Prepa inc","Prepa","Prof inc", "Prof","Posgrado")
barplot(edu,beside = T,col=c("deeppink4","darkturquoise"), main="Educación", legend.text = T,ylab = "Frecuencia absoluta",cex.names = 0.6)



#Vestido y calzado
plot(density(ahrr$vesti_calz, from=0), col="deeppink4", main="Vestido y calzado", lwd=2 , xlim=c(0,.3), xlab="Porcentaje",ylab = "Densidad")
lines(density(gast$vesti_calz, from=0), col="cadetblue4", lwd=2)
legend("topright", c("Ahorran", "Se endeudan"), col=c("deeppink4", "cadetblue4") , lty=c(1,1), lwd = c(2,2))


#Vivienda
plot(density(ahrr$vivienda, from=0), col="deeppink4", main="Vivienda", lwd=2 , xlim=c(0,.5), ylim=c(0,10), xlab="Porcentaje",ylab = "Densidad")
lines(density(gast$vivienda,from=0), col="cadetblue4", lwd=2)
legend("topright", c("Ahorran", "Se endeudan"), col=c("deeppink4", "cadetblue4") , lty=c(1,1), lwd = c(2,2))

#Alquiler
plot(density(ahrr$alquiler, from=0), col="deeppink4", main="Alquiler", lwd=2 , xlim=c(-.01,.2), ylim=c(0,55), xlab="Porcentaje",ylab = "Densidad")
lines(density(gast$alquiler, from=0), col="cadetblue4", lwd=2)
legend("topright", c("Ahorran", "Se endeudan"), col=c("deeppink4", "cadetblue4") , lty=c(1,1), lwd = c(2,2))

#Salud
plot(density(ahrr$salud, from = 0), col="deeppink4", main="Salud", lwd=2 , xlim=c(0,.04), ylim=c(0,120), xlab="Porcentaje",ylab = "Densidad")
lines(density(gast$salud ,from = 0), col="cadetblue4", lwd=2)
legend("topright", c("Ahorran", "Se endeudan"), col=c("deeppink4", "cadetblue4") , lty=c(1,1), lwd = c(2,2))

#Adquisisión vehiculo
plot(density(ahrr$adqui_vehi, from = 0), col="deeppink4", main="Adquisición de vehículo", lwd=2 , xlim=c(0,.1), ylim=c(0,90), xlab="Porcentaje",ylab = "Densidad")
lines(density(gast$adqui_vehi, from = 0), col="cadetblue4", lwd=2)
legend("topright", c("Ahorran", "Se endeudan"), col=c("deeppink4", "cadetblue4") , lty=c(1,1), lwd = c(2,2))

#Comunicación
plot(density(ahrr$comunica, from = 0), col="deeppink4", main="Comunicación", lwd=2 , xlim=c(0,.4), ylim=c(0,28), xlab="Porcentaje",ylab = "Densidad")
lines(density(gast$comunica, from = 0), col="cadetblue4", lwd=2)
legend("topright", c("Ahorran", "Se endeudan"), col=c("deeppink4", "cadetblue4") , lty=c(1,1), lwd = c(2,2))

#Esparcimiento
plot(density(ahrr$esparci, from = 0), col="deeppink4", main="Esparcimiento", lwd=2 , xlim=c(0,.1), ylim=c(0,75), xlab="Porcentaje",ylab = "Densidad")
lines(density(gast$esparci, from = 0), col="cadetblue4", lwd=2)
legend("topright", c("Ahorran", "Se endeudan"), col=c("deeppink4", "cadetblue4") , lty=c(1,1), lwd = c(2,2))

#Paquetes turisticos
plot(density(ahrr$paq_turist, from = 0), col="deeppink4", main="Paquetes Turísticos", lwd=2 , xlim=c(0,.03), ylim=c(0,150), xlab="Porcentaje",ylab = "Densidad")
lines(density(gast$paq_turist, from = 0), col="cadetblue4", lwd=2)
legend("topright", c("Ahorran", "Se endeudan"), col=c("deeppink4", "cadetblue4") , lty=c(1,1), lwd = c(2,2))

summary(gast)
summary(ahrr)
