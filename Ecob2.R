#Segunda Parte

#analisis de datos de 2016.
year<-2016
distancias<-fread("distancias.csv")
#load libraries
library(data.table)

#setwd
#setwd("/Users/Diana/Documents/Personal/Personal/LabCDMX/Ecobici/Ecobici")

#leer los datos
data<-fread(paste("ecobici",year,"_C.csv",sep=""),verbose=FALSE,showProgress=FALSE)

#vamos a ver cuántos viajes hay de origen a destino, necesitamos una matriz.
subd<-cbind(data$Ciclo_Estacion_Arribo,data$Ciclo_Estacion_Retiro)
subd<-as.data.frame(subd)
names(subd)<-c("Ciclo_Estacion_Arribo","Ciclo_Estacion_Retiro")
d2<-melt(subd,id.vars=c(1,2),na.rm=FALSE)

aristas<-dcast(d2, Ciclo_Estacion_Retiro + Ciclo_Estacion_Arribo ~ .)

#comprobar si si es lo que queremos
head(aristas)
ind1<-which(data$Ciclo_Estacion_Retiro==1)
ind2<-which(data$Ciclo_Estacion_Arribo==1)
length(intersect(ind1,ind2))


#hacemos una matriz de adyacencia con los pesos que encontramos
adj<-matrix(NA,nrow=dim(distancias)[1],ncol=dim(distancias)[2])

#la llenamos
for(i in 1:dim(aristas)[1])
{
  s<-aristas[i,1]
  t<-aristas[i,2]
  w<-aristas[i,3]
  adj[s,t]<-w
}

#para ver que tan vacía o llena esta la matriz se puede usar:
image(1:dim(distancias)[2],1:dim(distancias),adj)
#se puede ver que la matriz esta practicamente vacía. a partir de las estaciones xxx (approx)


#guardar el archivo
fwrite(aristas,file=paste("OrigenDestinoLista_",year,".csv",sep=""),row.names=FALSE)
adj<-as.data.frame(adj)
fwrite(adj,file=paste("OrigenDestinoMatriz_",year,".csv",sep=""),row.names=FALSE)

#leer cves delegaciones y colonias 
cves<-fread("cvesColoniasEstaciones.csv")
#matriz origen destino colonias 
cveCol<-fread("CveEstacion_Col.csv")
cveCol<-cveCol[order(cveCol[,1]),]
cveCol<-as.data.frame(cveCol)
#hacemos una matriz de adyacencia para colonias y para delegaciones
adjCol<-matrix(0,nrow=dim(cves)[1],ncol=dim(cves)[1])
adjDel<-matrix(0,nrow=length(unique(cves$Cve_Del)),ncol=length(unique(cves$Cve_Del)))


aristas<-fread(paste("OrigenDestinoLista_",year,".csv",sep=""))

i<-1
for(i in 1:dim(aristas)[1])
{
  s<-as.numeric(aristas[i,1])
  cols<-cveCol[s,2]
  aux<-which(cves$Cols==cols)
  cvecols<-as.numeric(cves[aux,2])
  cveDels<-as.numeric(cves[aux,4])
  
  t<-as.numeric(aristas[i,2])
  colt<-cveCol[t,2]
  aux2<-which(cves$Cols==colt)
  cvecolt<-as.numeric(cves[aux2,2])
  cveDelt<-as.numeric(cves[aux2,4])
  
  w<-as.numeric(aristas[i,3])
  aux<-as.numeric(adjCol[cvecols,cvecolt])
  adjCol[cvecols,cvecolt]<- aux + w
  aux2<-as.numeric(adjDel[cveDels,cveDelt])
  adjDel[cveDels,cveDelt]<- aux2 + w
}

adjDel<-as.data.frame(adjDel)
adjCol<-as.data.frame(adjCol)

fwrite(adjDel,file=paste("OD_Delegacion",year,".csv",sep=""))
fwrite(adjCol,file=paste("OD_Colonia",year,".csv",sep=""))


image(1:dim(adjDel)[2],1:dim(adjDel)[2],as.matrix(adjDel))
image(1:dim(adjCol)[2],1:dim(adjCol)[2],as.matrix(adjCol)

#estadistica descriptiva
#identificar plots principales


