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
  # #a??o
  # aux<-format(data$Hora_Retiro,'%Y')
  # data<-cbind(data,aux)
  # names(data)[length(names(data))]<-"Year"
  # class(data$Year)
  # data$Year<-as.numeric(levels(data$Year))[data$Year]

}
{
# horas<-as.POSIXct()
#
#
# strptime_C<-cmpfun(strptime)
# difftime_C<-cmpfun(difftime)
# as.ITime_C<-cmpfun(as.ITime)
#
#
# strp<-function(horas,horas2)
# {
# aux3<-strptime(horas,format="%H:%M:%S",tz="")
# aux4<-strptime(horas2,format="%H:%M:%S",tz="")
# interval<-difftime(aux4,aux3)
# return(list(aux3,aux4,interval))
# }
#
# strp2<-function(horas,horas2)
# {
#   aux3<-strptime_C(horas,format="%H:%M:%S",tz="")
#   aux4<-strptime_C(horas2,format="%H:%M:%S",tz="")
#   interval<-difftime_C(aux4,aux3)
#   return(list(aux3,aux4,interval))
# }
#
# Id<-function(horas,horas2)
# {
#   aux3p<-as.ITime(horas)
#   aux4p<-as.ITime(horas2)
#   interval2<-as.integer(aux4p)-as.integer(aux3p)
#   return(list(aux3p,aux4p,interval2))
# }
#
# Id2<-function(horas,horas2)
# {
#   aux3p<-as.ITime_C(horas)
#   aux4p<-as.ITime_C(horas2)
#   interval2<-as.integer(aux4p)-as.integer(aux3p)
#   return(list(aux3p,aux4p,interval2))
# }
#
# strp2_C<-cmpfun(strp2)
# Id2_C<-cmpfun(Id2)
#
# #system.time(strp(horas,horas2))
# #system.time(Id(horas,horas2))
# #user  system elapsed
# #217.580   1.554 220.682
# #> system.time(Id(horas,horas2))
# #user  system elapsed
# #216.324   0.892 218.333
# #system.time(strp2_C(horas,horas2))
# #system.time(Id2_C(horas,horas2))
# #user  system elapsed
# #213.434   1.450 216.225
# #> system.time(Id2_C(horas,horas2))
# #user  system elapsed
# #215.696   0.974 217.729
#
# system.time()
#
# aux4<-strptime(horas2,format="%H:%M:%S",tz="")
}


install.packages("data.table")
install.packages("igraph")
install.packages("reshape2")
install.packages("sp")
install.packages("geosphere")
install.packages("curl")
install.packages("parallel")
install.packages("fasttime")
rm(list=ls(all=TRUE))


library(reshape2)
library(igraph)
library(data.table)
library(geosphere)
library(curl)
library(parallel)
library(fasttime)

#strptime_C<-cmpfun(strptime)
#difftime_C<-cmpfun(difftime)
#Idate_C<-cmpfun(as.IDate)


#set working directory
setwd("/Users/JoseManuel/Documents/DianaMunch")



###Leer los datos de las localizaciones de las estaciones para hacer un c??lculo de la distancia de los recorridos recorrida
###calcular distancias
{

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
}




#leer varios conjuntos de datos:
#hacer una lista de las urls donde estan los datos separadas por comas y entre comillas. Tambi??n se puede leer un file en donde esten todas las direcciones. la instrucci??n para concatenar un vector es c(,,..,)
#hay datos desde feb 2010 hasta febrero 2017.
meses<-c("01" , "02",  "03" , "04" , "05" , "06" , "07",  "08",  "09" , "10" ,"11", "12")
a??os<-seq(2010,2017,1)
#ejemplo de liga a csv ecobici
#https://www.ecobici.cdmx.gob.mx/sites/default/files/data/usages/2017-01.csv

lista<-c()
for (a in a??os)
{
  for(m in meses)
  {
    s<-paste("https://www.ecobici.cdmx.gob.mx/sites/default/files/data/usages/",a,"-",m,".csv",sep="")
    lista<-c(lista,s)
  }}

#quitar los a??os / meses que no existen (esto se puede quitar con ~ try catch )
#lista<-lista[-c(1,87:96)]

l<-1
dat<-fread(lista[l],verbose=FALSE,showProgress=FALSE)
dat<-as.data.frame(dat)
for (l in 2:length(lista))
{
  mydat <- fread(lista[l],verbose=FALSE,showProgress=FALSE)
  mydat<-as.data.frame(mydat)
  dat<-rbind(dat,mydat)
  print(l)
}
fwrite(dat,file="ecobici.csv",col.names=FALSE)


#limpiarlos
##ver de que clase los leyo
year<-2016

data<-fread(paste("ecobici",year,".csv",sep=""))
data<-as.data.frame(data)
dim(data)

for( i in 1:dim(data)[2])
{
  print(names(data)[i])
  print(class(data[,i]))
}

###Separar por a??os para facilitar el handling en memoria
# a??os<-seq(2010,2017,1)
# year<-paste(a??os)
# y2<-substr(data$Fecha_Retiro,1,4)
# for (n in year)
# {
#   ind<-which(y2==n)
#   dat<-data[ind,]
#   fwrite(dat,file=paste("ecobici",n,".csv",sep=""),col.names=FALSE)
#   print(n)
# }
#

#Leer y manipular cada a??o de datos.
rm(list=ls(all=TRUE))
a??os<-seq(2010,2017,1)
a??os<-seq(2012,2017,1)
year<-paste(a??os)

n<-year[1]

for (n in year)
{
  data<-fread(file=paste("ecobici",n,".csv",sep=""))
  data<-as.data.frame(data)
  names(data)<-c("Genero_Usuario","Edad_Usuario","Bici","Ciclo_Estacion_Retiro","Fecha_Retiro","Hora_Retiro","Ciclo_Estacion_Arribo","Fecha_Arribo","Hora_Arribo")



#transformar los datos de fecha de Retiro de la bici pq son distintos algunos
  year<-rep(NA,dim(data)[1])
  month<-year
  day<-month
  vector<-data$Fecha_Retiro

  pos<-regexpr("/",vector)
  auxpos<-which(pos != -1 )
  upos<-setdiff(unique(pos),-1)
  split<-strsplit(vector[auxpos],"/",fixed=TRUE)
  split2<-strsplit(vector[-auxpos],"-",fixed=TRUE)

  if(upos == 3)
{
  if(!is.null(auxpos))
  {
  auxseq<-1:length(split)
  day[auxpos]<-as.numeric(unlist(lapply(auxseq,function(x) split[[x]][1])))
  month[auxpos]<-as.numeric(unlist(lapply(auxseq,function(x) split[[x]][2])))
  year[auxpos]<-as.numeric(unlist(lapply(auxseq,function(x) split[[x]][3])))
  }
  if(!is.null(day[-auxpos]))
  {
  auxseq<-1:length(split2)
  day[-auxpos]<-as.numeric(unlist(lapply(auxseq,function(x) split2[[x]][3])))
  month[-auxpos]<-as.numeric(unlist(lapply(auxseq,function(x) split2[[x]][2])))
  year[-auxpos]<-as.numeric(unlist(lapply(auxseq,function(x) split2[[x]][1])))
  }
}else{
  auxseq<-1:length(split)
  day[auxpos]<-as.numeric(unlist(lapply(auxseq,function(x) split[[x]][3])))
  month[auxpos]<-as.numeric(unlist(lapply(auxseq,function(x) split[[x]][2])))
  year[auxpos]<-as.numeric(unlist(lapply(auxseq,function(x) split[[x]][1])))
  auxseq<-1:length(split2)
  day[-auxpos]<-as.numeric(unlist(lapply(auxseq,function(x) split2[[x]][1])))
  month[-auxpos]<-as.numeric(unlist(lapply(auxseq,function(x) split2[[x]][2])))
  year[-auxpos]<-as.numeric(unlist(lapply(auxseq,function(x) split2[[x]][3])))
  print("POSIBLE ERROR FECHAS")
}

newdates<-as.Date(paste(day,"-",month,"-",year,sep=""),format="%d-%m-%Y")
data$Fecha_Retiro<-newdates

#cual era el dia de la semana
weekdays<-weekdays(newdates)
#Hora Truncada
Hora_T<-as.numeric(gsub(":","",substr(data$Hora_Retiro,1,2),fixed=TRUE))
#agregar datos
data<-cbind(data,year,month,day,Hora_T,weekdays)
names(data)<-c(names(data)[1:(dim(data)[2]-5)],"Year","Mes","dia_mes","Hora_T","dia_semana")
rm(auxpos,day,Hora_T,month,newdates,pos,split,split2,upos,vector,weekdays,year)


#Lo mismo para la fecha de arribo
year<-rep(NA,dim(data)[1])
month<-year
day<-month
vector<-data$Fecha_Arribo
pos<-regexpr("/",vector)
auxpos<-which(pos != -1 )
upos<-setdiff(unique(pos),-1)
split<-strsplit(vector[auxpos],"/",fixed=TRUE)
split2<-strsplit(vector[-auxpos],"-",fixed=TRUE)
if(upos == 3)
{
  if(!is.null(auxpos))
  {
    auxseq<-1:length(split)
    day[auxpos]<-as.numeric(unlist(lapply(auxseq,function(x) split[[x]][1])))
    month[auxpos]<-as.numeric(unlist(lapply(auxseq,function(x) split[[x]][2])))
    year[auxpos]<-as.numeric(unlist(lapply(auxseq,function(x) split[[x]][3])))
  }
  if(!is.null(day[-auxpos]))
  {
    auxseq<-1:length(split2)
    day[-auxpos]<-as.numeric(unlist(lapply(auxseq,function(x) split2[[x]][3])))
    month[-auxpos]<-as.numeric(unlist(lapply(auxseq,function(x) split2[[x]][2])))
    year[-auxpos]<-as.numeric(unlist(lapply(auxseq,function(x) split2[[x]][1])))
  }
}else{
  auxseq<-1:length(split)
  day[auxpos]<-as.numeric(unlist(lapply(auxseq,function(x) split[[x]][3])))
  month[auxpos]<-as.numeric(unlist(lapply(auxseq,function(x) split[[x]][2])))
  year[auxpos]<-as.numeric(unlist(lapply(auxseq,function(x) split[[x]][1])))
  auxseq<-1:length(split2)
  day[-auxpos]<-as.numeric(unlist(lapply(auxseq,function(x) split2[[x]][1])))
  month[-auxpos]<-as.numeric(unlist(lapply(auxseq,function(x) split2[[x]][2])))
  year[-auxpos]<-as.numeric(unlist(lapply(auxseq,function(x) split2[[x]][3])))
  print("POSIBLE ERROR FECHAS")
}
newdates<-as.Date(paste(day,"-",month,"-",year,sep=""),format="%d-%m-%Y")
data$Fecha_Arribo<-newdates
rm(auxseq,year,month,day,vector,auxpos,newdates,pos,split,split2,upos,vector,wee)

#duracion del viaje (Las horas tienen algunas un punto entonces hay que unificar los datos)
vector<-data$Hora_Retiro
split<-strsplit(vector,".",fixed=TRUE)
horas<-unlist(lapply(seq(1:length(split)),function(x) split[[x]][1]))

vector<-data$Hora_Arribo
split<-strsplit(vector,".",fixed=TRUE)
horas2<-unlist(lapply(seq(1:length(split)),function(x) split[[x]][1]))

aux<-1:length(horas)
#estas ser??an las fechas con hora completas, se necesita para calcular r??pidamente la duraci??n del viaje
horas.1<-fastPOSIXct(paste(data$Fecha_Retiro,horas),tz="UTC")
horas.2<-fastPOSIXct(paste(data$Fecha_Arribo,horas2),tz="UTC")

interval<-(horas.2-horas.1)/60
interval<-difftime(horas.2,horas.1)

data<-cbind(data,interval,horas.1,horas.2)
names(data)<-c(names(data)[1:(dim(data)[2]-3)],"duracion","FRetiro_Comp","FArribo_Comp")


rm(aux,horas,horas.1,horas.2,horas2,split,vector)

#viajes que regresaron a la misma estaci??n
aux2<-data$Ciclo_Estacion_Arribo==data$Ciclo_Estacion_Retiro
data<-cbind(data,aux2)
names(data)[dim(data)[2]]<-"Misma_Estacion"

rm(aux2)

#hay un tema de que algunas entradas tienen hora de entrega 0 que != a 00:00:00. Esto puede significar dos cosas, no entregaron la bici o la entregaron justo a la hora cero. Esto es importante saberlo
# numero estaciones con arribo o destino no identificados
#452 es el numero total de estaciones
ind1<-which(data$Ciclo_Estacion_Arribo>452)
ind2<-which(data$Ciclo_Estacion_Retiro>452)
#intersect(ind1,ind2)
#length(ind1)+length(ind2)-length(intersect(ind1,ind2))
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

rm(distance,distancias,aux,ind1,ind2,data)
gc()
}









