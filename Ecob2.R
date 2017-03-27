
#Segunda Parte

#analisis de datos de 2016.
year<-2016
distancias<-fread("distancias.csv")
#load libraries
library(data.table)

#setwd
setwd("/Users/Diana/Documents/Personal/Personal/LabCDMX/Ecobici/Ecobici")

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

View(adj)
#plot de grafica con edges por peso

g<-graph_from_adjacency_matrix(adj, mode = "directed", weighted = TRUE, diag = TRUE, add.colnames = NULL, add.rownames = NA)











#estadistica descriptiva
#identificar plots principales
#agregar el calculo con colonia o delegacion y con clima


#guardar el archivo
fwrite(aristas,file=paste("Aristas_",year,".csv",sep=""),row.names=FALSE)

