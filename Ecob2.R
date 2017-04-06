#Segunda Parte

#analisis de datos de 2016.
year<-2016

#load libraries
library(data.table)

distancias<-fread("distancias.csv")


#setwd()
setwd("/Users/JoseManuel/Documents/DianaMunch")


#leer los datos
data<-fread(paste("ecobici",year,"_C.csv",sep=""),verbose=FALSE,showProgress=FALSE)



#Origen Destino por Estaci??n, por colonia y por delegacion 

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

#para ver que tan vac??a o llena esta la matriz se puede usar:
#image(1:dim(distancias)[2],1:dim(distancias),log(adj)) #En matlab heatmap es mejor
#se puede ver donde tiene huecos la matriz.

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
t<-table(data$Hora_T, data$dia_semana)
mm<-max(t)
plot(seq(1,dim(t)[1],1),t[,1],type="l",xlim=c(0,24),ylim=c(0,mm))
lines(seq(1,dim(t)[1],1),t[,1],type="l",col="2")
lines(seq(1,dim(t)[1],1),t[,2],type="l",col="3")
lines(seq(1,dim(t)[1],1),t[,3],type="l",col="4")
lines(seq(1,dim(t)[1],1),t[,4],type="l",col="5")
lines(seq(1,dim(t)[1],1),t[,5],type="l",col="6")
lines(seq(1,dim(t)[1],1),t[,6],type="l",col="7")
lines(seq(1,dim(t)[1],1),t[,7],type="l",col="8")
legend("topright",col=c("2","3","4","5","6","7","8"),legend=c("F","M","Sa","Su","Th","Tu","Wed"),fill=c("2","3","4","5","6","7","8"),border="black")


#uso versus edadxhora edadxsemana?
quantiles<-quantile(data$Edad_Usuario,probs=seq(0,1,.2))
vector<-data$Edad_Usuario
n<-length(vector)
whichquant<-unlist(lapply((1:n), function(x) which.max(data$Edad_Usuario[x] < quantiles)))

#edad x hora 
usoedad<-table(data$Hora_T,whichquant)
usoedad[,2]<-usoedad[,2]+usoedad[,1]
usoedad<-usoedad[,-1]
colnames(usoedad)<-paste(1:5)

t<-usoedad/colSums(usoedad)
plot(seq(1,dim(t)[1],1),t[,1],xlim=c(0,23), ylim=c(0,max(t)),type="l")
lines(seq(1,dim(t)[1],1),t[,1],type="l",col="2")
lines(seq(1,dim(t)[1],1),t[,2],type="l",col="3")
lines(seq(1,dim(t)[1],1),t[,3],type="l",col="4")
lines(seq(1,dim(t)[1],1),t[,4],type="l",col="5")
lines(seq(1,dim(t)[1],1),t[,5],type="l",col="6")
legend("topright",col=c("2","3","4","5","6"),legend=c("<26","<30","<34","<43","<116"),fill=c("2","3","4","5","6","7","8"),border="black")

###uso edad dos (todas las edades no por cuantiles)
#primero hacer el color palette

color.palette <- function(steps, n.steps.between=NULL, ...){
  if(is.null(n.steps.between)) n.steps.between <- rep(0, (length(steps)-1))
  if(length(n.steps.between) != length(steps)-1) stop("Must have one less n.steps.between value than steps")
  fill.steps <- cumsum(rep(1, length(steps))+c(0,n.steps.between))
  RGB <- matrix(NA, nrow=3, ncol=fill.steps[length(fill.steps)])
  RGB[,fill.steps] <- col2rgb(steps)
  for(i in which(n.steps.between>0)){
    col.start=RGB[,fill.steps[i]]
    col.end=RGB[,fill.steps[i+1]]
    for(j in seq(3)){
      vals <- seq(col.start[j], col.end[j], length.out=n.steps.between[i]+2)[2:(2+n.steps.between[i]-1)]  
      RGB[j,(fill.steps[i]+1):(fill.steps[i+1]-1)] <- vals
    }
  }
  new.steps <- rgb(RGB[1,], RGB[2,], RGB[3,], maxColorValue = 255)
  pal <- colorRampPalette(new.steps, ...)
  return(pal)
}

steps <- c("blue4", "cyan", "snow3", "yellow", "red4")
pal <- color.palette(steps, c(20,1,1,20), space="rgb")
z=1:1000


usoedad<-table(data$Hora_T,data$Edad_Usuario)
ue45_50<-rowSums(usoedad[,30:35])
ue50_60<-rowSums(usoedad[,36:45])
ue_60<-rowSums(usoedad[,46:dim(usoedad)[2]])

usoedad2<-cbind(usoedad[,1:29],ue45_50,ue50_60,ue_60)
t<-t(apply(usoedad2,1,function(x) x/colSums(usoedad2)))*100
plot(seq(1,dim(t)[1],1),t[,1],xlim=c(0,25), ylim=c(0,max(t)),type="l")
for(i in 1:dim(t)[2])
{
lines(seq(1,dim(t)[1],1),t[,i],type="l",col=pal(50)[i])
}
mycolors<-unlist(lapply((1:dim(t)[2]),function(x) pal(50)[x]))
ejes<-sort(unique(data$Hora_T))
ejes<-c(ejes,24)
ejes<-ejes[seq(1,length(ejes),length.out=10)]

axis(1, at=ejes, labels=paste(ejes))
legend("topright",col=mycolors,legend=colnames(t),fill=mycolors,border="black",cex=.7)



# #edad x dia semana?  
# steps <- c("blue4", "cyan", "snow3", "yellow", "red4")
# pal <- color.palette(steps, c(20,1,1,20), space="rgb")
# z=1:1000
# mycolors<-unlist(lapply((1:dim(t)[2]),function(x) pal(115)[x]))
# t<-table(data$dia_semana,data$Edad_Usuario)
# csums<-colSums(t)
# rsums<-rowSums(t)
# t2<-apply(t,1,function(x) x/csums)
# 
# 
# #dejar la tabla en orden por dias de la semana (estan en orden alfabetico)
# t2<-t2[,c(2,6,7,5,1,3,4)]
# t<-t2
# 
# plot(seq(1,dim(t)[2],1),t[1,],xlim=c(1,7), ylim=c(0.05,.2),type="l")
# for(i in 1:dim(t)[1])
# {
#   lines(seq(1,dim(t)[2],1),t[i,],type="l",col=pal(50)[i])
# }
# 
# mycolors<-unlist(lapply((1:dim(t)[2]),function(x) pal(50)[x]))
# ejes<-sort(unique(data$Hora_T))
# ejes<-c(ejes,24)
# ejes<-ejes[seq(1,length(ejes),length.out=10)]
# 
# axis(1, at=ejes, labels=paste(ejes))
# legend("topright",col=mycolors,legend=rownames(t),fill=mycolors,border="black",cex=.7)
# 
# 
# t2<-t(apply(t,2,function(x) x/rsums))
# 
# barplot(t2,col=heat.colors())


#estaciones x sexo x porcentaje 
t<-table(data$Ciclo_Estacion_Arribo, names=data$Genero_Usuario)
sums<-colSums(t)




#VELOCIDADES
#Hay unos datos con duraci??n de viaje menor a 1 minuto y que si cambian de estacion,
#esto resulta en unas velocidades inverosimiles. Segun wikipedia el record es de 82km/hr por lo que consideraremos unicamente las menores a 80  
#cwe
mean(data$velocidad[which(velocidad<80)])




#Multas:
#Minuto 0 al 45Sin costo
#De 45 al 60.$11.00.
#Por hora o fracci??n adicional.$37.00.
#Reposici??n de tarjeta.$12.00.
#Uso mayor a 24 hrs. $5200.00.

multas<-length(which(data$duracion>45))
multas*100/dim(data)[1]

m45_60<-intersect(which(data$duracion>45),which(data$duracion<=60))
m61<-which(data$duracion>60)
length(m45_60)*11

horasextra<-data$duracion[m61]
horasextra<-(horasextra-45)/60
ind<-which(horasextra>24)

multasHorasextra<-sum(horasextra[-ind]*37)
multasDiasextra<-sum(5200*length(ind))

length(m61)
data$duracion[m61][1:10]




#calculo de watts (power / energy)
#2016: 90,961.35 D??as de tiempo de recorrido 
#velocidad promedio 8.6 km / hr 
#peso promedio de un mexicano: (63*.25+ 74*.75)=71.25 kg 
#equivale a approximadamente 6 kcal por minuto 

#por lo tanto como el dia tiene 1440 minutos = 6*1440*90961.35
#785,906,064 kcal 

#1 foco de 100 watts consume 1 kcal en 42 segundos 
#1 foco de 100 watts consume 785,906,064  en ( 785,906,064 * 42) segundos 

#un foco de 100 watts por 1046 a??os
#1000 focos de 100 wats por 1 a??o y 16 d??as  



#Leer los datos de todos los a??os y contar el n??mero de viajes v??lido y por h y mujeres
library(data.table)

annos<-seq(2011,2016,1)
annos<-paste(annos)
n<-annos[1]

mydat<-fread("ecobici2011_C.csv")
mydat<-as.data.frame(mydat)
names(mydat)<-c("Genero_Usuario","Edad_Usuario","Bici","Ciclo_Estacion_Retiro","Fecha_Retiro","Hora_Retiro","Ciclo_Estacion_Arribo","Fecha_Arribo","Hora_Arribo","Year","Mes","dia_mes","dia_semana","duracion","FRetiro_Comp","FArribo_Comp","HRetiro_C","HArribo_C","Hora_T","Misma_Estacion","distancia","velocidad")
mydat<-mydat[,c(1,2,10)]  

for(n in annos)
{
  data<-fread(file=paste("ecobici",n,"_C.csv",sep=""))
  data<-as.data.frame(data)
  data<-data[,c(1,2,10)]
  names(data)<-c("Genero_Usuario","Edad_Usuario","Year")
  mydat<-rbind(mydat,data)
}

fwrite(mydat,file="AnnosCompletos.csv")

data<-fread("AnnosCompletos.csv")

ind<-which(data$Genero_Usuario=="")
if(length(ind)>0){data<-data[-ind,]}
ind<-which(data$Year=="2010")
if(length(ind)>0){data<-data[-ind,]}

t<-table(data$Year,data$Genero_Usuario)

rsums<-rowSums(t)
csums<-colSums(t)
trows<-apply(t,2,function(x) x/rsums)
tcols<-t(apply(t,1,function(x) x/csums))
####


###matriz origen destino de la bici mas recorrida. 
#Bicicleta que se utiliz?? m??s 
library("reshape2")

distancia<-data$distancia
class(distancia)
bicis<-data$Bici
aux<-data.frame(bicis,distancia)
ind<-which(aux$distancia>5000)
aux<-aux[-ind,]
aux2<-melt(aux)
aux3<-dcast(aux2, bicis ~ ., sum) 
who<-which.max(aux3[,2])

bici<-as.numeric(levels(aux3[who,1]))[aux3[who,1]]

names(data)
class(data$Bici)
bicidata<-data[which(data$Bici==bici),]

class(bicidata)
bicidata<-as.data.frame(bicidata)

t<-table(bicidata$Ciclo_Estacion_Retiro,bicidata$Ciclo_Estacion_Arribo)
t<-as.data.frame(t)
fwrite(t, file= paste("OD_BICI", bici, ".csv", sep=""))
fwrite(bicidata,file=paste("Data_BICI", bici, ".csv", sep=""))


head(bicidata)





