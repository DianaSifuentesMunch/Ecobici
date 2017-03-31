#Segunda Parte

#analisis de datos de 2016.
year<-2016

#load libraries
library(data.table)

distancias<-fread("distancias.csv")
#setwd
#setwd("/Users/Diana/Documents/Personal/Personal/LabCDMX/Ecobici/Ecobici")

#leer los datos
data<-fread(paste("ecobici",year,"_C.csv",sep=""),verbose=FALSE,showProgress=FALSE)



#vamos a ver cu??ntos viajes hay de origen a destino, necesitamos una lista para hacer la grafica.
# subd<-cbind(data$Ciclo_Estacion_Arribo,data$Ciclo_Estacion_Retiro)
# subd<-as.data.frame(subd)
# names(subd)<-c("Ciclo_Estacion_Arribo","Ciclo_Estacion_Retiro")
# d2<-melt(subd,id.vars=c(1,2),na.rm=FALSE)
# aristas<-dcast(d2, Ciclo_Estacion_Retiro + Ciclo_Estacion_Arribo ~ .)
# #comprobar si si es lo que queremos
# head(aristas)
# ind1<-which(data$Ciclo_Estacion_Retiro==1)
# ind2<-which(data$Ciclo_Estacion_Arribo==1)
# length(intersect(ind1,ind2))
# fwrite(aristas,file=paste("OrigenDestinoLista_",year,".csv",sep=""),row.names=FALSE)

#
OD<-table(data$Ciclo_Estacion_Retiro,data$Ciclo_Estacion_Arribo)
#OD[1:10,1:10]
write.table(OD,file=paste("OrigenDestinoMatriz_",year,".csv",sep=""),sep=",")

OD2<-as.data.frame(OD)
#OD2[c(1:5,456:460),]
names(OD2)<-c("source","target","weight")
fwrite(OD2,file=paste("OrigenDestinoLista_",year,".csv",sep=""),row.names=FALSE)

#matriz de adyacencia con pesos para previsualizacion
adj<-matrix(OD2$weight,nrow=dim(distancias)[1],ncol=dim(distancias)[2],byrow=TRUE)

deciles<-quantile(as.numeric(adj),probs=seq(0,1,.05),na.rm=FALSE)
deciles
unique(deciles)

#para ver que tan vac??a o llena esta la matriz se puede usar:
#image(1:dim(distancias)[2],1:dim(distancias),log(adj)) #En matlab heatmap es mejor
#se puede ver donde tiene huecos la matriz.
#guardar el archivo

#leer cves delegaciones y colonias
cves<-fread("cvesColoniasEstaciones.csv")
#matriz origen destino colonias
cveCol<-fread("CveEstacion_Col.csv")
cveCol<-cveCol[order(cveCol[,1]),]
cveCol<-as.data.frame(cveCol)

#hacemos una matriz de adyacencia para colonias y para delegaciones
class(data$Ciclo_Estacion_Retiro)

colSource<-cveCol[data$Ciclo_Estacion_Retiro,2]
colDestino<-cveCol[data$Ciclo_Estacion_Arribo,2]

ODCol<-table(colSource,colDestino)
write.table(ODCol,file=paste("OrigenDestinoColoniasMatriz_",year,".csv",sep=""),sep=",")
ODCol2<-as.data.frame(ODCol)
#OD2[c(1:5,456:460),]
names(ODCol2)<-c("source","target","weight")
fwrite(ODCol2,file=paste("OrigenDestinoColoniasLista_",year,".csv",sep=""),row.names=FALSE)


#hacemos una matriz de adyacencia para delegaciones
DelSource<-match( colSource, cves$Cols)
DelDestino<-match( colDestino, cves$Cols)
DelSource<-cves$Del[DelSource]
DelDestino<-cves$Del[DelDestino]
ODDel<-table(DelSource,DelDestino)

write.table(ODDel,file=paste("OrigenDestinoDelsMatriz_",year,".csv",sep=""),sep=",")

ODDel2<-as.data.frame(ODDel)
#OD2[c(1:5,456:460),]
names(ODDel2)<-c("source","target","weight")
fwrite(ODDel2,file=paste("OrigenDestinoDelsLista_",year,".csv",sep=""),row.names=FALSE)


#estadistica descriptiva

#distancia por viaje promedio

indexF<-intersect(which(data$distancia!=0),which(data$Genero_Usuario=="F"))
mean(data$distancia[indexF])
summary(data$distancia[indexF])

indexM<-intersect(which(data$distancia!=0),which(data$Genero_Usuario=="M"))
mean(data$distancia[indexM])
summary(data$distancia[indexM])

summary(data$distancia[which(data$distancia!=0)])
#1653.927 metros
#distancia total 
sum(data$distancia)/1000
#14,603,825 km

#distancias totales por sexo 
sum(data$distancia[indexF])/1000
#3494157
sum(data$distancia[indexH])/1000
# 11109668

#distancias por Edad 
sum(data$distancia[indexF])/1000
#3494157
sum(data$distancia[indexH])/1000
# 11109668



#d??a de la semana de uso de bici
sem<-table(data$dia_semana)
sem[c(2,6,7,5,1,3,4)]/sum(sem)

#   Friday     Monday   Saturday     Sunday   Thursday    Tuesday  Wednesday
#0.16522777 0.16557302 0.07603575 0.06314005 0.17625761 0.18038718 0.17337861

#dia de la semana de uso de bici por sexo
semsex1<-table(data$dia_semana,names=data$Genero_Usuario)
semsex<-cbind(semsex1[,1]/sum(semsex1[,1]),semsex1[,2]/sum(semsex1[,2]))
colnames(semsex)<-c("F","M")
semsex<-semsex[c(2,6,7,5,1,3,4),]
semsex1<-semsex1[c(2,6,7,5,1,3,4),]


#F          M
#Monday    0.16248714 0.16659667
#Tuesday   0.17716477 0.18145611
#Wednesday 0.16990387 0.17453125
#Thursday  0.17187470 0.17771151
#Friday    0.15933257 0.16718332
#Saturday  0.08317323 0.07366811
#Sunday    0.07606373 0.05885302

#plot(semsex)

options(digits = 2)
#uso por hora vs sexo
hrsex1<-table(data$Hora_T,names=data$Genero_Usuario)
hrsex<-cbind(hrsex1[,1]/sum(hrsex1[,1]),hrsex1[,2]/sum(hrsex1[,2]))
colnames(hrsex)<-c("F","M")
print(hrsex*100)

#tabla de usos por hora y dia de la semana 
t<-plot(seq(1,dim(d)[1],1),t[,1],type="l")

lines(seq(1,dim(d)[1],1),t[,1],type="l",col="2")
lines(seq(1,dim(d)[1],1),t[,2],type="l",col="3")
lines(seq(1,dim(d)[1],1),t[,3],type="l",col="4")
lines(seq(1,dim(d)[1],1),t[,4],type="l",col="5")
lines(seq(1,dim(d)[1],1),t[,5],type="l",col="6")
lines(seq(1,dim(d)[1],1),t[,6],type="l",col="7")
lines(seq(1,dim(d)[1],1),t[,7],type="l",col="8")
table(data$Hora_T, data$dia_semana)

#uso versus edad. 


#Hay unos datos con duraci??n de viaje menor a 1 minuto y que si cambian de estacion,
#esto resulta en unas velocidades inverosimiles. Segun wikipedia el record es de 82km/hr por lo que consideraremos unicamente las menores a 80  
#cwe
mean(data$velocidad[which(velocidad<80)])

#Multas:

multas<-length(which(data$duracion>45))
multas/dim(data)[1]
.29%