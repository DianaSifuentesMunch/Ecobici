install.packages("data.table")
install.packages("reshape2")
install.packages("sp")
install.packages("geosphere")
install.packages("curl")
install.packages("parallel")
install.packages("fasttime")
rm(list=ls(all=TRUE))


library(reshape2)
library(data.table)
library(geosphere)
library(curl)
library(parallel)
library(fasttime)

setwd("/Users/JoseManuel/Documents/DianaMunch")


###calcular distancias entre estacion y estacion

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

#leer los datos por a??o y por mes:
#hacer una lista de las urls donde estan los datos separadas por comas y entre comillas. Tambinnn se puede leer un file en donde esten todas las direcciones. la instruccinnn para concatenar un vector es c(,,..,)
#hay datos desde feb 2010 hasta febrero 2017.
meses<-c("01" , "02",  "03" , "04" , "05" , "06" , "07",  "08",  "09" , "10" ,"11", "12")
annos<-seq(2011,2016,1)

#para leer meses por separado hacer por ejemplo: meses<-c("01") annos<-"2011"

#leer los datos 
#este pedazo lee y guarda los datos como estan:   
  for (a in annos)
  {
    lista<-c()
    for(m in meses)
    {
      s<-paste("https://www.ecobici.cdmx.gob.mx/sites/default/files/data/usages/",a,"-",m,".csv",sep="")
      lista<-c(lista,s)
    }

l<-1
dat<-fread(lista[l],verbose=FALSE,showProgress=FALSE)
dat<-as.data.frame(dat)
for (l in 2:length(lista))
{
  mydat <- fread(lista[l],verbose=FALSE,showProgress=FALSE)
  mydat<-as.data.frame(mydat)
  dat<-rbind(dat,mydat)
  print(a)
  print(l)
  }
fwrite(dat,file=paste("ecobici_",a,".csv",sep=""),col.names=FALSE)
}




#limpiar los datos 
#Leer y manipular cada anno de datos.
rm(list=ls(all=TRUE))
annos<-seq(2011,2016,1)
annos<-paste(annos)
n<-annos[1]

for (n in annos)
{
  data<-fread(file=paste("ecobici_",n,".csv",sep=""))
  data<-as.data.frame(data)
  names(data)<-c("Genero_Usuario","Edad_Usuario","Bici","Ciclo_Estacion_Retiro","Fecha_Retiro","Hora_Retiro","Ciclo_Estacion_Arribo","Fecha_Arribo","Hora_Arribo")
  
  #transformar los datos de fecha de Retiro de la bici pq son distintos algunos
  year<-rep(NA,dim(data)[1])
  month<-year
  day<-month
  vector<-data$Fecha_Retiro
  
  pos<-regexpr("/",vector)
  pos2<-regexpr("-",vector)
  auxpos<-which(pos != -1 )
  auxpos2<-which(pos2 != -1 )
  
  bandera<-FALSE
  upos<-setdiff(unique(pos),-1)

  split<-strsplit(vector[auxpos],"/",fixed=TRUE)
  split2<-strsplit(vector[auxpos2],"-",fixed=TRUE)  
  

  if(length(upos)==0 & length(auxpos2)==dim(data)[1])
  {
    auxseq<-1:length(split2)
    day[auxpos2]<-as.numeric(unlist(lapply(auxseq,function(x) split2[[x]][3])))
    month[auxpos2]<-as.numeric(unlist(lapply(auxseq,function(x) split2[[x]][2])))
    year[auxpos2]<-as.numeric(unlist(lapply(auxseq,function(x) split2[[x]][1])))
    bandera<-TRUE  
  }
  
  if(length(upos)==dim(data)[1]  & length(auxpos2)==0)
  {
  auxseq<-1:length(split)
  day[auxpos]<-as.numeric(unlist(lapply(auxseq,function(x) split[[x]][1])))
  month[auxpos]<-as.numeric(unlist(lapply(auxseq,function(x) split[[x]][2])))
  year[auxpos]<-as.numeric(unlist(lapply(auxseq,function(x) split[[x]][3])))
  bandera<-TRUE
  }
  
  if(upos == 3 && ! bandera)
  {
    if(length(auxpos)!=0)
    {
      auxseq<-1:length(split)
      day[auxpos]<-as.numeric(unlist(lapply(auxseq,function(x) split[[x]][1])))
      month[auxpos]<-as.numeric(unlist(lapply(auxseq,function(x) split[[x]][2])))
      year[auxpos]<-as.numeric(unlist(lapply(auxseq,function(x) split[[x]][3])))
    }
    if(length(day[auxpos2])!=0)
    {
      auxseq<-1:length(split2)
      day[auxpos2]<-as.numeric(unlist(lapply(auxseq,function(x) split2[[x]][3])))
      month[auxpos2]<-as.numeric(unlist(lapply(auxseq,function(x) split2[[x]][2])))
      year[auxpos2]<-as.numeric(unlist(lapply(auxseq,function(x) split2[[x]][1])))
    }
  }else{
    if(! bandera) #aqui entra solo si los separadores "/" y "-" estan en formato tipo 2015/03/02 y 03-02-2015
    {
    auxseq<-1:length(split)
    day[auxpos]<-as.numeric(unlist(lapply(auxseq,function(x) split[[x]][3])))
    month[auxpos]<-as.numeric(unlist(lapply(auxseq,function(x) split[[x]][2])))
    year[auxpos]<-as.numeric(unlist(lapply(auxseq,function(x) split[[x]][1])))
    auxseq<-1:length(split2)
    day[auxpos2]<-as.numeric(unlist(lapply(auxseq,function(x) split2[[x]][1])))
    month[auxpos2]<-as.numeric(unlist(lapply(auxseq,function(x) split2[[x]][2])))
    year[auxpos2]<-as.numeric(unlist(lapply(auxseq,function(x) split2[[x]][3])))
    print("POSIBLE ERROR FECHAS")
  }
  }
  newdates<-as.Date(paste(day,"-",month,"-",year,sep=""),format="%d-%m-%Y")
  data$Fecha_Retiro<-newdates
  
  #cual era el dia de la semana
  weekdays<-weekdays(newdates)
  #agregar datos
  data<-cbind(data,year,month,day,weekdays)
  names(data)<-c(names(data)[1:(dim(data)[2]-4)],"Year","Mes","dia_mes","dia_semana")
  rm(auxpos,day,month,newdates,pos,split,split2,upos,vector,weekdays,year,auxpos2)
  
  #Lo mismo para la fecha de arribo
  year<-rep(NA,dim(data)[1])
  month<-year
  day<-month
  vector<-data$Fecha_Arribo
  
  pos<-regexpr("/",vector)
  pos2<-regexpr("-",vector)
  auxpos<-which(pos != -1 )
  auxpos2<-which(pos2 != -1 )

  bandera<-FALSE
  upos<-setdiff(unique(pos),-1)
  
  split<-strsplit(vector[auxpos],"/",fixed=TRUE)
  split2<-strsplit(vector[auxpos2],"-",fixed=TRUE)  
  
  
  if(length(upos)==0 && length(auxpos2)==dim(data)[1])
  {
    split2<-strsplit(vector[auxpos2],"-",fixed=TRUE)  
    auxseq<-1:length(split2)
    day[auxpos2]<-as.numeric(unlist(lapply(auxseq,function(x) split2[[x]][3])))
    month[auxpos2]<-as.numeric(unlist(lapply(auxseq,function(x) split2[[x]][2])))
    year[auxpos2]<-as.numeric(unlist(lapply(auxseq,function(x) split2[[x]][1])))
    bandera<-TRUE
  }
  
  if(length(upos)==dim(data)[1]  && length(auxpos2)==0)
  {
    split<-strsplit(vector[auxpos],"/",fixed=TRUE)
    auxseq<-1:length(split)
    day[auxpos]<-as.numeric(unlist(lapply(auxseq,function(x) split[[x]][1])))
    month[auxpos]<-as.numeric(unlist(lapply(auxseq,function(x) split[[x]][2])))
    year[auxpos]<-as.numeric(unlist(lapply(auxseq,function(x) split[[x]][3])))
    bandera<-TRUE
    
  }
  
  if(upos == 3 && !(bandera))
  {
    if(length(auxpos)!=0)
    {
      auxseq<-1:length(split)
      day[auxpos]<-as.numeric(unlist(lapply(auxseq,function(x) split[[x]][1])))
      month[auxpos]<-as.numeric(unlist(lapply(auxseq,function(x) split[[x]][2])))
      year[auxpos]<-as.numeric(unlist(lapply(auxseq,function(x) split[[x]][3])))
    }
    if(length(day[auxpos2])!=0)
    {
      auxseq<-1:length(split2)
      day[auxpos2]<-as.numeric(unlist(lapply(auxseq,function(x) split2[[x]][3])))
      month[auxpos2]<-as.numeric(unlist(lapply(auxseq,function(x) split2[[x]][2])))
      year[auxpos2]<-as.numeric(unlist(lapply(auxseq,function(x) split2[[x]][1])))
    }
  }else{
    if(!(bandera)){
    auxseq<-1:length(split)
    day[auxpos]<-as.numeric(unlist(lapply(auxseq,function(x) split[[x]][3])))
    month[auxpos]<-as.numeric(unlist(lapply(auxseq,function(x) split[[x]][2])))
    year[auxpos]<-as.numeric(unlist(lapply(auxseq,function(x) split[[x]][1])))
    auxseq<-1:length(split2)
    day[auxpos2]<-as.numeric(unlist(lapply(auxseq,function(x) split2[[x]][1])))
    month[auxpos2]<-as.numeric(unlist(lapply(auxseq,function(x) split2[[x]][2])))
    year[auxpos2]<-as.numeric(unlist(lapply(auxseq,function(x) split2[[x]][3])))
    print("POSIBLE ERROR FECHAS")
  }
  }  

  newdates<-as.Date(paste(day,"-",month,"-",year,sep=""),format="%d-%m-%Y")
  data$Fecha_Arribo<-newdates
  rm(auxseq,year,month,day,vector,auxpos,newdates,pos,split,split2,upos,auxpos2)
  
  #duracion del viaje (Las horas tienen algunas un puntos y estan en distintos formatos 24hrs o 12 hrs entonces hay que unificar el formato)
  vector<-data$Hora_Retiro
  puntos<-grepl(".",vector,fixed=TRUE)
  AM<-grepl("AM",vector,fixed=TRUE)
  PM<-grepl("PM",vector,fixed=TRUE)
  nvoHRetiro<-vector
  
# quitar los puntos
if(sum(puntos)!=0)
{
  split<-strsplit(vector[puntos],".",fixed=TRUE)
  puntos2<-unlist(lapply(seq(1:length(split)),function(x) split[[x]][1]))
  nvoHRetiro[which(puntos)]<-puntos2
}

#hacerlos formato 24 hrs     
if( (sum(AM)+sum(PM) ) != 0)
  {
  index_conversion<-sort(unique(c(which(AM),which(PM))))
  nvos_times<-strptime(vector[index_conversion],"%I:%M:%S %p")
  nvos_times<-paste(nvos_times)
  nvos_times<-strsplit(nvos_times," ")
  nvos_times<-unlist(lapply(1:length(nvos_times),function(x) nvos_times[[x]][2]))
  nvoHRetiro[index_conversion]<-nvos_times
  }

#poner las que tienen fecha de 0:00:00 en 00:00:00

  index<-which(nvoHRetiro=="0:00:00")
  if(length(index)!=0){nvoHRetiro[index]<-"00:00:00"}
    
  ###Lo mismo para la hora de Arribo 
  vector<-data$Hora_Arribo
  puntos<-grepl(".",vector,fixed=TRUE)
  AM<-grepl("AM",vector,fixed=TRUE)
  PM<-grepl("PM",vector,fixed=TRUE)
  nvoHArribo<-vector
  #puntos
  if(sum(puntos)!=0)
  {
    split<-strsplit(vector[puntos],".",fixed=TRUE)
    puntos2<-unlist(lapply(seq(1:length(split)),function(x) split[[x]][1]))
    nvoHArribo[which(puntos)]<-puntos2
  }
  #quitar los AM y PM y dejar formato 24 hrs
  if( (sum(AM)+sum(PM) ) != 0)
  {
    index_conversion<-sort(unique(c(which(AM),which(PM))))
    nvos_times<-strptime(vector[index_conversion],"%I:%M:%S %p")
    nvos_times<-paste(nvos_times)
    nvos_times<-strsplit(nvos_times," ")
    nvos_times<-unlist(lapply(1:length(nvos_times),function(x) nvos_times[[x]][2]))
    nvoHArribo[index_conversion]<-nvos_times
  }
  
  index<-which(nvoHArribo=="0:00:00")
  index2<-which(nvoHArribo=="00:00:00")
  index<-unique(c(index,index2))
  
  if(length(index)!=0){nvoHArribo[index]<-"24:00:00"}
  
  #estas sernnan las fechas con hora completas, se necesita para calcular rnnpidamente la duracinnn del viaje
  horas.1<-fastPOSIXct(paste(data$Fecha_Retiro,nvoHRetiro),tz="UTC")
  horas.2<-fastPOSIXct(paste(data$Fecha_Arribo,nvoHArribo),tz="UTC")
  interval<-(horas.2-horas.1)/60
  data<-cbind(data,interval,horas.1,horas.2,nvoHRetiro,nvoHArribo)
  names(data)<-c(names(data)[1:(dim(data)[2]-5)],"duracion","FRetiro_Comp","FArribo_Comp","HRetiro_C","HArribo_C")
  
  data$duracion<-as.numeric(data$duracion)
  
  #Hora Truncada de salida
  Hora_T<-as.numeric(gsub(":","",substr(data$HRetiro_C,1,2),fixed=TRUE))
  data<-cbind(data,Hora_T)
  names(data)<-c(names(data)[1:(dim(data)[2]-1)],"Hora_T")
  
  rm(horas.1,horas.2,vector,interval,nvoHArribo,nvoHRetiro,PM,AM,puntos,Hora_T)
  
  #viajes que regresaron a la misma estacinnn
  aux2<-data$Ciclo_Estacion_Arribo==data$Ciclo_Estacion_Retiro
  data<-cbind(data,aux2)
  names(data)[dim(data)[2]]<-"Misma_Estacion"
  
  rm(aux2)
  
  # numero estaciones con arribo o destino no identificados
  #452 es el numero total de estaciones
  ind1<-which(data$Ciclo_Estacion_Arribo>452)
  ind2<-which(data$Ciclo_Estacion_Retiro>452)
  #quitar las entradas con esta estacion desconocida
  aux<-unique(sort(c(ind1,ind2)))
  if(length(aux)!=0){
    data<-data[-aux,]}
  
  #calcular distancia del viaje
  distancias<-fread("distancias.csv")
  distancias<-as.matrix(distancias)
  
  distance<-unlist(lapply(seq(1:dim(data)[1]),function(x) distancias[data$Ciclo_Estacion_Retiro[x],data$Ciclo_Estacion_Arribo[x]]))
  #agregarl la columna a nuestro database
  data<-cbind(data,distance)
  names(data)[dim(data)[2]]<-"distancia"

  velocidad<-60/1000*(data$distancia/data$duracion)
  data<-cbind(data,velocidad)
  names(data)[dim(data)[2]]<-"velocidad"
  
  
  fwrite(data,file=paste("ecobici",n,"_C",".csv",sep=""),row.names=FALSE,col.names=TRUE)
  
  rm(distance,distancias,aux,ind1,ind2,data)
  gc()
}

