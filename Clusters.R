library(cluster)
library(corrplot)
library(FactoMineR)
library(bpca)
library(rgl)
library(scatterplot3d)
datos<-read.csv("~/Documents/Data Mining/concentradohogaror.csv", header = T,sep = ",")

#porcentaje<-(datos$ing_cor/datos$gasto_mon)
#porcentaje1<-(datos[,c(59)])
#table(datos$gasto_mon>=datos$ing_cor)
#Quitamos los datos que tienen 0 en gastos monetarios, pues no podemos dividir entre 0
noc<-which(datos$gasto_mon==0)
datos<-datos[-noc,]
datoscont<-datos[,-c(1,2:12,13:58,60:76,78,79,81:84,86:88,90:92,94:100,102:104,106:109,110:127)] #conservamos solo las variables continuas
attach(datos)
datoscont[,c(1:8)] <-datos[,c(59,77,80,85,89,93,101,105)]/gasto_mon
#datoscont[,c(1,2,3)]<-datos[,c(25,48,56)]/ing_cor
View(datoscont)

distancia <- sqrt(mahalanobis(x = datoscont, center = colMeans(datoscont), cov = cov(datoscont)))
plot(distancia, xlab = "Sujetos", ylab = "Distancia de Mahalanobis", main = "Detección de datos atípicos",
     col = "palevioletred4", pch = 19, ylim = c(0, 25))
abline(h = sqrt(qchisq(0.99, 8)), col = "red2", lwd = 3)

ati<-which(distancia>sqrt(qchisq(0.99, 8)))

datos1<-datoscont[-ati,]

m1<-datos1[sample(x = nrow(datos1), size = .10*nrow(datos1), replace = FALSE),]

#View(m1)


###############Analisis descriptivo#########
par(mfrow=c(1,2))
hist(datos1$alimentos,main = "Alimentos", col = "cadetblue4",ylab = "Frecuencia",xlab = "Porcentaje") 
boxplot(datos1$alimentos, main = "Alimentos", col = "cadetblue4") 

hist(datos1$vesti_calz, main = "Vestido y calzado", col = "cadetblue4",ylab = "Frecuencia",xlab = "Porcentaje") 
boxplot(datos1$vesti_calz, main = "Vestido y calzado", col = "cadetblue4") 

hist(datos1$vivienda, main = "Vivienda", col = "cadetblue4",ylab = "Frecuencia",xlab = "Porcentaje") 
boxplot(datos1$vivienda, main = "Vivienda", col = "cadetblue4") 

hist(datos1$limpieza, main = "Limpieza", col = "cadetblue4",ylab = "Frecuencia",xlab = "Porcentaje") 
boxplot(datos1$limpieza, main = "Limpieza", col = "cadetblue4")

hist(datos1$salud, main = "Cuidados de la salud", col = "cadetblue4",ylab = "Frecuencia",xlab = "Porcentaje") 
boxplot(datos1$salud, main = "Cuidados de la salud", col = "cadetblue4") 

hist(datos1$transporte, main = "Transporte", col = "cadetblue4",ylab = "Frecuencia",xlab = "Porcentaje") 
boxplot(datos1$transporte, main = "Transporte", col = "cadetblue4") 

hist(datos1$educa_espa, main = "Educación y esparcimiento", col = "cadetblue4",ylab = "Frecuencia",xlab = "Porcentaje",cex.main=0.9) 
boxplot(datos1$educa_espa, main = "Educación y esparcimiento", col = "cadetblue4",cex.main=0.9) 

hist(datos1$personales, main = "Personales", col = "cadetblue4",ylab = "Frecuencia",xlab = "Porcentaje")
boxplot(datos1$personales, main = "Personales", col = "cadetblue4") 
summary(datos1)
###############Distancia euclideana##########################
dist1=dist(m1, method = "euclidean")
table(is.na(dist1))
#average
promedio1<- hclust(dist1, method = "average")
##simple
simple1<- hclust(dist1, method = "single")
#compuesta
compuesta1<- hclust(dist1, method = "complete")
#ward
ward1<-hclust(dist1,method = "ward.D")

###Average
vect1.1<-c(0)
for(i in 2:100){
  division1.1 <- cutree(promedio1, k = i)
  s<-summary(silhouette(division1.1, dist1))
  vect1.1<-c(vect1.1,s$avg.width)
}

which(vect1.1==max(vect1.1))
plot(vect1.1)
vect1.1[2]


division1.1.1 <- cutree(promedio1, k = 2)
plot(silhouette(division1.1.1, dist1), main="Promedio-euclidiana",col="blue")
x<-cbind(m1,division1.1.1)
x1<-subset(x,division1.1.1==1)
x2<-subset(x,division1.1.1==2)

which(x$division1.1.1==2)
summary(x2[,-9])
summary(x1[,-9])


##Complete
compuesta<- hclust(dist1, method = "complete")
vect1.3<-c(0)
for(i in 2:100){
  division1.3 <- cutree(compuesta, k = i)
  s<-summary(silhouette(division1.3, dist1))
  vect1.3<-c(vect1.3,s$avg.width)
}

which(vect1.3==max(vect1.3))

division1.1.3 <- cutree(compuesta, k = 2)
plot(silhouette(division1.1.3, dist1), main="Complete-euclidiana",col="coral2")

#Ward
vect1.4<-c(0)
for(i in 2:100){
  division1.4 <- cutree(ward1, k = i)
  s<-summary(silhouette(division1.4, dist1))
  vect1.4<-c(vect1.4,s$avg.width)
}

which(vect1.4==max(vect1.4))
vect1.4[2]

division1.4.1 <- cutree(ward1, k = 2)
plot(silhouette(division1.4.1, dist1), main="Ward-euclidiana",col="coral2")

w<-cbind(m1,division1.4.1)
w1<-subset(w,division1.4.1==1)
w2<-subset(w,division1.4.1==2)


summary(w2[,-9])
summary(w1[,-9])



###############Distancia maxium##########################
dist2=dist(m1, method = "maximum")
#average
promedio2<- hclust(dist2, method = "average")
#compuesta
compuesta2<- hclust(dist2, method = "complete")
#ward
ward2<-hclust(dist2,method = "ward.D")


##Silhouette plot
#Average
vect2.1<-c(0)
for(i in 2:100){
  division2.1 <- cutree(promedio2, k = i)
  s<-summary(silhouette(division2.1, dist2))
  vect2.1<-c(vect2.1,s$avg.width)
}
#plot(vect2.1)
which(vect2.1==max(vect2.1))


division2.1.1 <- cutree(promedio2, k = 2)
plot(silhouette(division2.1.1, dist2), main="Promedio-maxium",col="coral2")


###Complete
vect2.3<-c(0)
for(i in 2:100){
  division2.3 <- cutree(compuesta2, k = i)
  s<-summary(silhouette(division2.3, dist2))
  vect2.3<-c(vect2.3,s$avg.width)
}
plot(vect2.3)

which(vect2.3==max(vect2.3)) 
vect2.3[2]

division2.3.1 <- cutree(compuesta2, k = 2)
plot(silhouette(division2.3.1, dist2), main="Complete-Maxium",col="coral2")

#Ward
vect2.4<-c(0)
for(i in 2:200){
  division2.4 <- cutree(ward2, k = i)
  s<-summary(silhouette(division2.4, dist2))
  vect2.4<-c(vect2.4,s$avg.width)
}

which(vect2.4==max(vect2.4))
vect2.4[2]

division2.4.1 <- cutree(ward2, k = 2)
plot(silhouette(division2.4.1, dist2), main="Ward-maxium",col="coral2")


w<-cbind(m1,division2.4.1)
w1<-subset(w,division2.4.1==1)
w2<-subset(w,division2.4.1==2)
w1<-w1[,-9]
w2<-w2[,-9]


comp<-PCA(m1,scale.unit = FALSE)
comp$eig
#Vector de colores
color1<-as.character(division2.4.1)
color1[color1=="1"]="steelblue"
color1[color1=="2"]="green"
plot(bpca::bpca(m1, d = 1:3, meth = "sqrt"), main = "Biplot con S", xlim = c(-5, 5), ylim = c(-5, 5), xlab = "Componente 1", ylab = "Componente 2",zlab = "Componente3", var.color = "purple",
     obj.cex = 0.8, var.cex = 1, obj.col = color1, obj.pos = 4, obj.offset = -1, lwd = 3)



plot(bpca(m1, d = 1:3, meth = "sqrt"), rgl.use = TRUE, var.cex = 1.5, obj.cex = 1.5, obj.col =color1, lwd = 5, obj.pos = 4, main = "Biplot con S")


###############Distancia Manhattan#################

dist3=dist(m1, method = "manhattan")
#average
promedio3<- hclust(dist3, method = "average")
#compuesta
compuesta3<- hclust(dist3, method = "complete")
#ward
ward3<-hclust(dist3,method = "ward.D")

##Silhouette plot
#Average
vect3.1<-c(0)
for(i in 2:100){
  division3.1 <- cutree(promedio3, k = i)
  s<-summary(silhouette(division3.1, dist3))
  vect3.1<-c(vect3.1,s$avg.width)
}

which(vect3.1==max(vect3.1))
vect3.1[2]

division3.1.1 <- cutree(promedio3, k = 2)
plot(silhouette(division3.1.1, dist3), main="Promedio-Manhattan",col="coral2")



###Complete
vect3.3<-c(0)
for(i in 2:200){
  division3.3 <- cutree(compuesta3, k = i)
  s<-summary(silhouette(division3.3, dist3))
  vect3.3<-c(vect3.3,s$avg.width)
}

which(vect3.3==max(vect3.3))
vect3.3[2]

division3.3.1 <- cutree(compuesta3, k = 2)
plot(silhouette(division3.3.1, dist3), main="Complete-Manhattan",col="coral2")

#Ward
vect3.4<-c(0)
for(i in 2:200){
  division3.4 <- cutree(ward3, k = i)
  s<-summary(silhouette(division3.4, dist3))
  vect3.4<-c(vect3.4,s$avg.width)
}

which(vect3.4==max(vect3.4))
vect3.4[2]

division3.4.1 <- cutree(ward3, k = 2)
plot(silhouette(division3.4.1, dist3), main="Ward-Manhattan",col="coral2")


###############Analisis de Componentes principales######
comp<-PCA(m1,scale.unit = FALSE)
comp$eig
plot(bpca::bpca(m1, d = 1:3, meth = "sqrt"), main = "Biplot con S", xlim = c(-5, 5), ylim = c(-5, 5), xlab = "Componente 1", ylab = "Componente 2",zlab = "Componente3", var.color = "darkred",
     obj.cex = 0.8, var.cex = 1, obj.col = , obj.pos = 4, obj.offset = -1, lwd = 4)

plot(bpca(datoscont, d = 1:3, meth = "sqrt"), rgl.use = TRUE, var.cex = 1.5, obj.cex = 1.5, obj.col = rainbow(8, start = .1), lwd = 5, obj.pos = 4, main = "Biplot con S")

base1<-base1[,-c(1:5)]
comp1<-PCA(base1,scale.unit = FALSE)
comp1$eig

distancia <- sqrt(mahalanobis(x = base1, center = colMeans(base1), cov = cov(base1)))
plot(distancia, xlab = "Sujetos", ylab = "Distancia de Mahalanobis", main = "Detección de datos atípicos",
     col = "palevioletred4", pch = 19, ylim = c(0, 25))
abline(h = sqrt(qchisq(0.99, 8)), col = "red2", lwd = 3)

ati<-which(distancia>sqrt(qchisq(0.99, 8)))

base1<-base1[-ati,]

m1<-datos1[sample(x = nrow(base1), size = .10*nrow(datos1), replace = FALSE),]

