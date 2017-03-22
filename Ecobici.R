{
# t1<-c()
# t2<-c()
#
# sec<-seq(1,dim(data)[1],50000)
# for(i in 1:length(sec))
# {
# if(i == length(sec)){
# 										inf<-sec[i]
# 										sup<-dim(data)[1]
# }else{
# 			inf<-sec[i]
# 			sup<-sec[(i+1)]-1
# 			}
#
# aux<-seq(inf,sup,1)
# aux2<-unlist(lapply(aux,function(x) paste(data[x,5],data[x,6],sep=" ")))
# aux2<-substr(aux2,1,19)
# aux2<-strptime(aux2,format="%Y-%m-%d %H:%M:%S",tz="")
# t1<-c(t1,aux2)
# aux3<-unlist(lapply(aux,function(x) paste(data[x,8],data[x,9],sep=" ")))
# aux3<-substr(aux3,1,19)
# aux3<-strptime(aux3,format="%Y-%m-%d %H:%M:%S",tz="")
# t2<-c(t2,aux3)
# print(i)
# }
# aux<-format(data$Hora_Retiro,'%H')
# data<-cbind(data,aux)
# names(data)[length(names(data))]<-"Hora_T"
# data$Hora_T<-as.numeric(levels(data$Hora_T))[data$Hora_T]
# #mes
# aux<-format(data$Hora_Retiro,'%M')
# data<-cbind(data,aux)
# names(data)[length(names(data))]<-"Mes"
# class(data$Mes)
# #data$Mes<-as.numeric(levels(data$Hora_T))[data$Hora_T]
# #año
# aux<-format(data$Hora_Retiro,'%Y')
# data<-cbind(data,aux)
# names(data)[length(names(data))]<-"Year"
# class(data$Year)
# data$Year<-as.numeric(levels(data$Year))[data$Year]

	}

#install.packages("data.table")
#install.packages("igraph")
#install.packages("reshape2")
#install.packages("sp")
#install.packages("geosphere")

library(reshape2)
library(igraph)
library(data.table)
library(geosphere)



#set working directory
setwd("/Users/Diana/Documents/Personal/Personal/LABCDMX/Ecobici/Ecobici")

###Leer los datos de las localizaciones de las estaciones para hacer un cálculo de la distancia de los recorridos recorrida
###calcular distancias

data<-read.table(file="estaciones.csv", header = TRUE, sep = ",", quote = "\"'", dec = ".", as.is = TRUE)
index<-order(data[,1])
data<-data[index,]
data<-cbind(data$id,data$location.lat,data$location.lon,data$nearbyStations.0,data$nearbyStations.1,data$nearbyStations.2,data$nearbyStations.3)
data<-as.data.frame(data)
names(data)<-c("id","lat","lon","C1","C2","C3","C4")

distancias<-distm(cbind(data$lon,data$lat),cbind(data$lon,data$lat),fun=distHaversine)

distancias<-fread("distancias.csv")
distancias<-distancias[,-1]
fwrite(distancias,file="distancias.csv",col.names=FALSE)

#leer varios conjuntos de datos:
#hacer una lista de las urls donde estan los datos separadas por comas y entre comillas. También se puede leer un file en donde esten todas las direcciones. la instrucción para concatenar un vector es c(,,..,)
#hay datos desde feb 2010 hasta febrero 2017.
meses<-c("01" , "02",  "03" , "04" , "05" , "06" , "07",  "08",  "09" , "10" ,"11", "12")
años<-seq(2010,2017,1)
#ejemplo de liga a csv ecobici
#https://www.ecobici.cdmx.gob.mx/sites/default/files/data/usages/2017-01.csv

lista<-c()
l2<-c()
for (a in años)
{
for(m in meses)
{
s<-paste("https://www.ecobici.cdmx.gob.mx/sites/default/files/data/usages/",a,"-",m,".csv",sep="")
l2<-c(l2,paste(a,m,sep=" "))
lista<-c(lista,s)
}}

#quitar los años / meses que no existen (esto se puede quitar con un try catch )
lista<-lista[-c(1,87:96)]
dat<-fread(lista[l],verbose=FALSE,showProgress=FALSE)
dat<-as.data.frame(dat)
for (l in lista)
{
mydat <- fread(lista[l])
mydat<-as.data.frame(mydat)
dat<-rbind(dat,mydat)
print(i)
}
fwrite(dat,file="ecobici.csv",col.names=FALSE)


#limpiarlos
##ver de que clase los leyo
data<-fread("ecobici.csv")
data<-as.data.frame(data)
dim(data)

for( i in 1:dim(data)[2])
{
print(names(data)[i])
print(class(data[,i]))
}
#data<-data[,-1]

###Separar por años para facilitar el handling en memoria
años<-seq(2010,2017,1)
year<-paste(años)
y2<-substr(data$Fecha_Retiro,1,4)
for (n in year)
{
	ind<-which(y2==n)
	dat<-data[ind,]
	fwrite(dat,file=paste("ecobici",n,".csv",sep=""),col.names=FALSE)
	print(n)
}


#Leer y manipular cada año de datos.
rm(list=ls(all=TRUE))

años<-seq(2010,2017,1)
year<-paste(años)
n<-year[2]
dim(distancias)

for (n in year)
{
data<-fread(file=paste("ecobici",n,".csv",sep=""))
data<-as.data.frame(data)
names(data)<-c("Genero_Usuario","Edad_Usuario","Bici","Ciclo_Estacion_Retiro","Fecha_Retiro","Hora_Retiro","Ciclo_Estacion_Arribo","Fecha_Arribo","Hora_Arribo")

#transformar los datos de tiempo
year<-substr(data$Fecha_Retiro,1,4)
month<-substr(data$Fecha_Retiro,6,7)
Hora_T<-substr(data$Hora_Retiro,1,2)

data<-cbind(data,year,month,Hora_T)
names(data)<-c(names(data)[1:(dim(data)[2]-3)],"Year","Mes","Hora_T")

rm(year,month,Hora_T)

#cual era el dia de la semana
weekdays<-weekdays(as.Date(data$Fecha_Retiro))
#duracion del viaje
#difftime(Hora_Arribo[1],Hora_Retiro[1])
aux<-seq(1:dim(data)[1])

aux1<-unlist(lapply(seq(1:dim(data)[1]),function(x) substr(data$Hora_Retiro[x],1,8)))
aux2<-unlist(lapply(seq(1:dim(data)[1]),function(x) substr(data$Hora_Arribo[x],1,8)))
aux3<-strptime(aux1,format="%H:%M:%S",tz="")
aux4<-strptime(aux2,format="%H:%M:%S",tz="")

interval<-sapply(aux,function(x) difftime(aux4[x],aux3[x]))

data<-cbind(data,interval)
data<-cbind(data,weekdays)
data$weekdays<-as.character(data$weekdays)
names(data)<-c(names(data)[1:(dim(data)[2]-2)],"duracion","dia_semana")
dia<-substr(data$Fecha_Retiro,9,10)
data<-cbind(data,dia)
names(data)[length(names(data))]<-"dia_mes"

rm(dia,weekdays,interval,aux3,aux4,aux2,aux1)

#viajes que regresaron a la misma estación
aux2<-sapply(aux,function(x) data$Ciclo_Estacion_Arribo[x]==data$Ciclo_Estacion_Retiro[x])
data<-cbind(data,aux2)
names(data)[dim(data)[2]]<-"Misma_Estacion"


#hay un tema de que algunas entradas tienen hora de entrega 0 que != a 00:00:00. Esto puede significar dos cosas, no entregaron la bici o la entregaron justo a la hora cero. Esto es importante saberlo

# numero estaciones con arribo o destino no identificados
#452 es el numero total de estaciones
ind1<-which(data$Ciclo_Estacion_Arribo>452)
ind2<-which(data$Ciclo_Estacion_Retiro>452)


intersect(ind1,ind2)
length(ind1)+length(ind2)-length(intersect(ind1,ind2))
#quitar las entradas con esta estacion desconocida
aux<-unique(sort(c(ind1,ind2)))
length(aux)
if(length(aux)!=0){
data<-data[-aux,]}

#calcular distancia del viaje
distancias<-fread("distancias.csv")
distancias<-as.matrix(distancias)
distance<-unlist(lapply(seq(1:dim(data)[1]),function(x) distancias[data$Ciclo_Estacion_Retiro[x],data$Ciclo_Estacion_Arribo[x]]))
#agregarl la columna a nuestro database
data<-cbind(data,distance)
names(data)[dim(data)[2]]<-"distancia"

fwrite(data,file=paste("ecobici",n,"_C",".csv",sep=""),row.names=FALSE,col.names=TRUE)

}








#vamos a ver cuántos viajes hay de origen a destino, necesitamos una matriz.

for( i in 1:dim(data)[2])
{
print(names(data)[i])
print(class(data[,i]))
}




nombres<-c("Genero_Usuario" ,  "Edad_Usuario" , "Bici" , "Ciclo_Estacion_Retiro", "Ciclo_Estacion_Arribo", "Hora_Retiro" ,"Hora_Arribo"  , "duracion" , "dia_semana", "Misma_Estacion","Hora_T" , "distancia")

id.v<-c("Genero_Usuario" , "Bici" , "Ciclo_Estacion_Retiro", "Ciclo_Estacion_Arribo", "dia_semana", "Hora_T")


id.v<-c("Ciclo_Estacion_Retiro", "Ciclo_Estacion_Arribo")

id.v<-c("Genero_Usuario" ,  "Edad_Usuario" , "Bici" , "Ciclo_Estacion_Retiro", "Ciclo_Estacion_Arribo", "Hora_Retiro" ,"Hora_Arribo"  , "duracion" , "dia_semana", "Misma_Estacion","Hora_T" , "distancia")


#measure.v<-c("Edad_Usuario","Hora_Retiro","Hora_Arribo","duracion","Hora_T","distancia","Misma_Estacion","dia_semana")
d2<-melt(data,id.vars=id.v,na.rm=FALSE)
aristas<-dcast(d2, Ciclo_Estacion_Retiro * Ciclo_Estacion_Arribo ~ .)
head(aristas)
ind1<-which(data$Ciclo_Estacion_Retiro==1)
ind2<-which(data$Ciclo_Estacion_Arribo==1)
length(intersect(ind1,ind2))

names(aristas)<-c("Source","Target","Weight")

#write.csv(aristas,file="aristas.csv",row.names=FALSE)
#hacemos una matriz de adyacencia con los pesos que encontramos

adj<-matrix(NA,nrow=dim(distancias)[1],ncol=dim(distancias)[2])
image(seq(1:452),seq(1:452),adj)



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


#plot de grafica con edges por peso

g<-graph_from_adjacency_matrix(adj, mode = "directed", weighted = TRUE, diag = TRUE, add.colnames = NULL, add.rownames = NA)











#estadistica descriptiva
#identificar plots principales
#agregar el calculo con colonia o delegacion y con clima
