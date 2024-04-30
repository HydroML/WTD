#test for just the conf/unconf raster
rm(list = ls())
library(raster)
files<-list.files("C:/Users/amini/Desktop/Ardalan_Paper_WTD/Rasters",full.names = T)
bigRas<-raster(files[19])
nrows<-dim(bigRas)[1]


dir.create("C:/Users/amini/Desktop/Ardalan_Paper_WTD/RasterRows")
library(foreach)
library(doParallel)
registerDoParallel(cores = 12)
getDoParWorkers()
cl <- makeCluster(12)
registerDoParallel(cl)
foreach(i=1:nrows,.packages='raster') %dopar% {
  cells <- cellFromRow(bigRas,i)
  vals<-getValues(bigRas,row=i)
  xy <- xyFromCell(bigRas, cells)
  dat<-cbind(xy,vals)
  dat<-dat[complete.cases(dat),]
  if(nrow(dat)>0){
    dimnames(dat)<-NULL
    saveRDS(dat,file = paste0("C:/Users/amini/Desktop/Ardalan_Paper_WTD/RasterRows/Row",i))
  }
}
stopCluster(cl)
rm(list=ls())


library(stringr)
library(data.table)
files<-list.files("C:/Users/amini/Desktop/Ardalan_Paper_WTD/RasterRows",full.names = T)

dat<-list()
for(i in 1:length(files)){
  curdat<-as.data.frame(readRDS(files[i]))
  curdat<-curdat[complete.cases(curdat),]
  dat[[i]]<-curdat
  print(i)
}
dat<-rbindlist(dat)


#start real thing


rm(list = ls())
library(raster)
files<-list.files("C:/Users/amini/Desktop/Ardalan_Paper_WTD/Rasters",full.names = T)
files<-files[-c(4,9,21:24)]
bigRas<-raster(files[1])
for(i in 2:length(files)){
  rCur<-raster(files[i])
  bigRas<-stack(bigRas,rCur)
}
rm(rCur,files,i)

nrows<-dim(bigRas)[1]
dir.create("C:/Users/amini/Desktop/Ardalan_Paper_WTD/RasterRows")



library(foreach)
library(doParallel)
registerDoParallel(cores = 12)
getDoParWorkers()
cl <- makeCluster(12)
registerDoParallel(cl)
foreach(i=1:nrows,.packages='raster') %dopar% {
  cells <- cellFromRow(bigRas,i)
  vals<-getValues(bigRas,row=i)
  xy <- xyFromCell(bigRas, cells)
  dat<-cbind(xy,vals)
  dat<-dat[complete.cases(dat) | (dat[,2]<60 & dat[,2]>48.3 & dat[,1]> -139 & dat[,1]< -114),]
  if(nrow(dat)>0){
    dimnames(dat)<-NULL
    saveRDS(dat,file = paste0("C:/Users/amini/Desktop/Ardalan_Paper_WTD/RasterRows/Row",i))
  }
}
stopCluster(cl)


rm(list=ls())
library(stringr)
library(data.table)
files<-list.files("C:/Users/amini/Desktop/Ardalan_Paper_WTD/RasterRows",full.names = T)
rasters<-list.files("C:/Users/amini/Desktop/Ardalan_Paper_WTD/Rasters",full.names = F)[-c(4,9,21:24)]
names<-c("lon","lat",substr(rasters,start=1,stop = str_length(rasters)-4))

dat<-list()
for(i in 1:length(files)){
  curdat<-as.data.frame(readRDS(files[i]))
  colnames(curdat)<-names
  #curdat<-curdat[!is.na(curdat$USGS_Conf_Unconf_Ext),]
  dat[[i]]<-curdat
  print(i)
}
dat<-rbindlist(dat)
saveRDS(dat,file = "C:/Users/amini/Desktop/Ardalan_Paper_WTD/dat.Rds")




rm(list = ls())
library(readxl)
dat<-readRDS("C:/Users/amini/Desktop/Ardalan_Paper_WTD/dat.Rds")

USGS_Wells_20220216 <- read.csv("C:/Users/amini/Desktop/Ardalan_Paper_WTD/USGS_Wells_20220216.csv")
Province_Conf_Unconf <- read_excel("C:/Users/amini/Desktop/Ardalan_Paper_WTD/Province_Conf_Unconf.xlsx")


USGS_Wells_20220216<-USGS_Wells_20220216[USGS_Wells_20220216$Confined.U!=5,]
USGS_Wells_20220216$Confined.U[USGS_Wells_20220216$Confined.U==2]<-1
USGS_Wells_20220216$Confined.U[USGS_Wells_20220216$Confined.U %in% c(3,4)]<-0

colnames(USGS_Wells_20220216)<-c("WTD","Unconfined","WellDepth","DTB","Slope","Elevation","AI","lat","lon")

USGS_Wells_20220216$SnowFrac<-NA
USGS_Wells_20220216$Prec<-NA
USGS_Wells_20220216$Pekel<-NA
USGS_Wells_20220216$Ks<-NA
USGS_Wells_20220216$Ks_Bed<-NA
USGS_Wells_20220216$Ks_std<-NA
for(i in 1:nrow(USGS_Wells_20220216)){
  w<-which.min((USGS_Wells_20220216$lat[i]-dat$lat)^2+(USGS_Wells_20220216$lon[i]-dat$lon)^2)
  USGS_Wells_20220216$SnowFrac[i]<-dat$Snowfrac1979_2009_EMDNA[w]
  USGS_Wells_20220216$Prec[i]<-dat$Prec1979_2009_EMDNA[w]
  USGS_Wells_20220216$Pekel[i]<-dat$Pekel_Occurence[w]
  USGS_Wells_20220216$Ks[i]<-dat$Ks[w]
  USGS_Wells_20220216$Ks_Bed[i]<-dat$Ks_Bed_GLHYMPS[w]
  USGS_Wells_20220216$Ks_std[i]<-dat$Ks_std[w]
  print(i)
}
rm(dat,i,w)






#0 is unconfined, 1 is confined
library(rpart)
library(rpart.plot)
treemod<-rpart(Unconfined~Elevation+Ks+Ks_Bed+AI+Ks_std+Slope+DTB+Prec+SnowFrac+Pekel+WellDepth,
               data = USGS_Wells_20220216,control =  rpart.control(maxdepth = 3,cp=.01))

treemod$variable.importance
rpart.plot(treemod)
predictions<-predict(treemod)
thresh<-.5
predictions[predictions<=thresh]<-0
predictions[predictions>thresh]<-1
sum(predictions==1 & USGS_Wells_20220216$Unconfined==1)
sum(predictions==0 & USGS_Wells_20220216$Unconfined==0)
sum(predictions==0 & USGS_Wells_20220216$Unconfined==1)
sum(predictions==1 & USGS_Wells_20220216$Unconfined==0)
sum(predictions== USGS_Wells_20220216$Unconfined)/nrow(USGS_Wells_20220216)
sum(predictions==0 & USGS_Wells_20220216$Unconfined==0)/sum(predictions==0)



#try global optimization
library(evtree)
gotree<-evtree(Unconfined~Elevation+Ks+Ks_Bed+AI+DTB+Prec+SnowFrac+WellDepth,
               data = USGS_Wells_20220216,control = evtree.control(maxdepth = 2,
                                                                   niterations = 1000, 
                                                                   ntrees = 100))

gotree[[1]]
predictionsO<-as.numeric(predict(gotree,USGS_Wells_20220216))
thresh<-.5
predictionsO[predictionsO<=thresh]<-0
predictionsO[predictionsO>thresh]<-1
sum(predictionsO==1 & USGS_Wells_20220216$Unconfined==1)
sum(predictionsO==0 & USGS_Wells_20220216$Unconfined==0)
sum(predictionsO==0 & USGS_Wells_20220216$Unconfined==1)
sum(predictionsO==1 & USGS_Wells_20220216$Unconfined==0)
sum(predictionsO== USGS_Wells_20220216$Unconfined)/nrow(USGS_Wells_20220216)
sum(predictionsO==0 & USGS_Wells_20220216$Unconfined==0)/sum(predictionsO==0)

#check a bunch of options with validation set
Train<-USGS_Wells_20220216[USGS_Wells_20220216$lat<45 | USGS_Wells_20220216$lat>49,]
Test<-USGS_Wells_20220216[USGS_Wells_20220216$lat>=45 & USGS_Wells_20220216$lat<49,]

library(rpart)
library(rpart.plot)
treemod<-rpart(Unconfined~Elevation+Ks+Ks_Bed+AI+Ks_std+Slope+DTB+Prec+SnowFrac+Pekel+WellDepth,
               data = Train,control =  rpart.control(maxdepth = 3,cp=.01))

treemod$variable.importance
rpart.plot(treemod)
predictions<-predict(treemod,Test)
thresh<-.5
predictions[predictions<=thresh]<-0
predictions[predictions>thresh]<-1
sum(predictions==1 & Test$Unconfined==1)
sum(predictions==0 & Test$Unconfined==0)
sum(predictions==0 & Test$Unconfined==1)
sum(predictions==1 & Test$Unconfined==0)
sum(predictions== Test$Unconfined)/nrow(Test)
sum(predictions==0 & Test$Unconfined==0)/sum(predictions==0)



#check regular tree with cross validation
nfold=5
overall<-0
unconfacy<-0
folder<-sample(1:nfold,nrow(USGS_Wells_20220216),replace = T)
for(fold in 1:nfold){
  Train<-USGS_Wells_20220216[folder!=fold,]
  Test<-USGS_Wells_20220216[folder==fold,]
  treemod<-rpart(Unconfined~Elevation+Ks+Ks_Bed+AI+Ks_std+Slope+DTB+Prec+SnowFrac+Pekel+WellDepth,
                 data = Train,control =  rpart.control(maxdepth = 2,cp=.01))
  predictions<-predict(treemod,Test)
  
  thresh<-.5
  predictions[predictions<=thresh]<-0
  predictions[predictions>thresh]<-1
  overall<-overall+sum(predictions== Test$Unconfined)/nrow(Test)
  unconfacy<-unconfacy+sum(predictions==0 & Test$Unconfined==0)/sum(predictions==0)
}
overall/nfold
unconfacy/nfold


#optimal decision tree
Train<-USGS_Wells_20220216[USGS_Wells_20220216$lat<45,]
Test<-USGS_Wells_20220216[USGS_Wells_20220216$lat>=45,]

gotree<-evtree(Unconfined~Elevation+Ks+Ks_Bed+AI+DTB+Prec+SnowFrac+WellDepth,
               data = Train,control = evtree.control(maxdepth = 2,
                                                                   niterations = 1000, 
                                                                   ntrees = 100))

gotree[[1]]
predictionsO<-as.numeric(predict(gotree,Test))
thresh<-.5
predictionsO[predictionsO<=thresh]<-0
predictionsO[predictionsO>thresh]<-1
sum(predictionsO==1 & Test$Unconfined==1)
sum(predictionsO==0 & Test$Unconfined==0)
sum(predictionsO==0 & Test$Unconfined==1)
sum(predictionsO==1 & Test$Unconfined==0)
sum(predictionsO== Test$Unconfined)/nrow(Test)
sum(predictionsO==0 & Test$Unconfined==0)/sum(predictionsO==0)


#check optimal tree with cross validation
library(evtree)
nfold=5
overall<-0
unconfacy<-0
folder<-sample(1:nfold,nrow(USGS_Wells_20220216),replace = T)
for(fold in 1:nfold){
  Train<-USGS_Wells_20220216[folder!=fold,]
  Test<-USGS_Wells_20220216[folder==fold,]
  gotree<-evtree(Unconfined~Elevation+Ks+Ks_Bed+AI+DTB+Prec+SnowFrac+WellDepth,
                 data = Train,control = evtree.control(maxdepth = 3,
                                                                     niterations = 1000, 
                                                                     ntrees = 100))
  
  predictions<-as.numeric(predict(gotree,Test))
  
  thresh<-.5
  predictions[predictions<=thresh]<-0
  predictions[predictions>thresh]<-1
  overall<-overall+sum(predictions== Test$Unconfined)/nrow(Test)
  unconfacy<-unconfacy+sum(predictions==0 & Test$Unconfined==0)/sum(predictions==0)
}
overall/nfold
unconfacy/nfold


#################################################################################3
##################################################################################
#test with canadian data 
#################################################################################
###################################################################################
rm(list = ls())
library(readxl)
dat<-readRDS("C:/Users/amini/Desktop/Ardalan_Paper_WTD/dat.Rds")

USGS_Wells_20220216 <- read.csv("C:/Users/amini/Desktop/Ardalan_Paper_WTD/USGS_Wells_20220216.csv")
Province_Conf_Unconf <- read_excel("C:/Users/amini/Desktop/Ardalan_Paper_WTD/Province_Conf_Unconf.xlsx")


USGS_Wells_20220216<-USGS_Wells_20220216[USGS_Wells_20220216$Confined.U!=5,]
USGS_Wells_20220216$Confined.U[USGS_Wells_20220216$Confined.U==2]<-1
USGS_Wells_20220216$Confined.U[USGS_Wells_20220216$Confined.U %in% c(3,4)]<-0

colnames(USGS_Wells_20220216)<-c("WTD","Unconfined","WellDepth","DTB","Slope","Elevation","AI","lat","lon")
USGS_Wells_20220216$DTB<-NULL
USGS_Wells_20220216$Slope<-NULL
USGS_Wells_20220216$Elevation<-NULL
USGS_Wells_20220216$AI<-NULL

colnames(Province_Conf_Unconf)<-c("lat","lon","WellDepth","WTD","remove","Unconfined")
Province_Conf_Unconf$remove<-NULL
Province_Conf_Unconf<-Province_Conf_Unconf[,c(4,5,3,1,2)]

USGS_Wells_20220216$country<-"US"
Province_Conf_Unconf$country<-"Canada"
ALL<-rbind(USGS_Wells_20220216,Province_Conf_Unconf)
rm(USGS_Wells_20220216,Province_Conf_Unconf)


ALL$SnowFrac<-NA
ALL$Prec<-NA
ALL$AI<-NA

ALL<-ALL[!is.na(ALL$lat),]


for(i in 1:nrow(ALL)){
  w<-which.min((ALL$lat[i]-dat$lat)^2+(ALL$lon[i]-dat$lon)^2)
  ALL$SnowFrac[i]<-dat$Snowfrac1979_2009_EMDNA[w]
  ALL$Prec[i]<-dat$Prec1979_2009_EMDNA[w]
  ALL$AI[i]<-dat$AI_Terra_EMDNA[w]
  print(i)
}
rm(dat,i,w)


saveRDS(ALL,file = "C:/Users/amini/Desktop/Ardalan_Paper_WTD/ALL.Rds")

rm(list = ls())
library(rpart)
library(rpart.plot)
USGS_Wells_20220216<-readRDS("C:/Users/amini/Desktop/Ardalan_Paper_WTD/ALL.Rds")

#check regular tree with cross validation
treemod<-rpart(Unconfined~AI+Prec+SnowFrac+WellDepth,
               data = USGS_Wells_20220216,control =  rpart.control(maxdepth = 3,cp=.01))

treemod$variable.importance
rpart.plot(treemod)
nfold=5
overall<-0
unconfacy<-0
folder<-sample(1:nfold,nrow(USGS_Wells_20220216),replace = T)
for(fold in 1:nfold){
  Train<-USGS_Wells_20220216[folder!=fold,]
  Test<-USGS_Wells_20220216[folder==fold,]
  treemod<-rpart(Unconfined~AI+Prec+SnowFrac+WellDepth,
                 data = Train,control =  rpart.control(maxdepth = 3,cp=.01))
  predictions<-predict(treemod,Test)
  
  thresh<-.5
  predictions[predictions<=thresh]<-0
  predictions[predictions>thresh]<-1
  overall<-overall+sum(predictions== Test$Unconfined)/nrow(Test)
  unconfacy<-unconfacy+sum(predictions==0 & Test$Unconfined==0)/sum(predictions==0)
}
overall/nfold
unconfacy/nfold

Train<-USGS_Wells_20220216[USGS_Wells_20220216$lat<45 | USGS_Wells_20220216$lat>49,]
Test<-USGS_Wells_20220216[USGS_Wells_20220216$lat>=45 & USGS_Wells_20220216$lat<49,]

library(rpart)
library(rpart.plot)
treemod<-rpart(Unconfined~AI+Prec+SnowFrac+WellDepth,
               data = Train,control =  rpart.control(maxdepth = 2,cp=.01))

treemod$variable.importance
rpart.plot(treemod)
predictions<-predict(treemod,Test)
thresh<-.5
predictions[predictions<=thresh]<-0
predictions[predictions>thresh]<-1
sum(predictions==1 & Test$Unconfined==1)
sum(predictions==0 & Test$Unconfined==0)
sum(predictions==0 & Test$Unconfined==1)
sum(predictions==1 & Test$Unconfined==0)
sum(predictions== Test$Unconfined)/nrow(Test)
sum(predictions==0 & Test$Unconfined==0)/sum(predictions==0)


# optimal decision tree
library(evtree)
gotree<-evtree(Unconfined~AI+Prec+SnowFrac+WellDepth,
               data = USGS_Wells_20220216,control = evtree.control(maxdepth = 3,
                                                                   niterations = 1000, 
                                                                   ntrees = 100))

gotree[[1]]


Train<-USGS_Wells_20220216[USGS_Wells_20220216$lat<45,]
Test<-USGS_Wells_20220216[USGS_Wells_20220216$lat>=45,]

gotree<-evtree(Unconfined~AI+Prec+SnowFrac+WellDepth,
               data = Train,control = evtree.control(maxdepth = 3,
                                                     niterations = 1000, 
                                                     ntrees = 100))

predictionsO<-as.numeric(predict(gotree,Test))
thresh<-.5
predictionsO[predictionsO<=thresh]<-0
predictionsO[predictionsO>thresh]<-1
sum(predictionsO==1 & Test$Unconfined==1)
sum(predictionsO==0 & Test$Unconfined==0)
sum(predictionsO==0 & Test$Unconfined==1)
sum(predictionsO==1 & Test$Unconfined==0)
sum(predictionsO== Test$Unconfined)/nrow(Test)
sum(predictionsO==0 & Test$Unconfined==0)/sum(predictionsO==0)


nfold=5
overall<-0
unconfacy<-0
folder<-sample(1:nfold,nrow(USGS_Wells_20220216),replace = T)
for(fold in 1:nfold){
  Train<-USGS_Wells_20220216[folder!=fold,]
  Test<-USGS_Wells_20220216[folder==fold,]
  gotree<-evtree(Unconfined~AI+Prec+SnowFrac+WellDepth,
                 data = Train,control = evtree.control(maxdepth = 3,
                                                       niterations = 1000, 
                                                       ntrees = 100))
  
  predictions<-as.numeric(predict(gotree,Test))
  
  thresh<-.5
  predictions[predictions<=thresh]<-0
  predictions[predictions>thresh]<-1
  overall<-overall+sum(predictions== Test$Unconfined)/nrow(Test)
  unconfacy<-unconfacy+sum(predictions==0 & Test$Unconfined==0)/sum(predictions==0)
}
overall/nfold
unconfacy/nfold



################################################################################
################# final predictions ###########################################
#################################################################################
library(evtree)
library(raster)
library(readxl)
rm(list = ls())

#import data
Wells_Canada_20220227 <- read.csv("C:/Users/amini/Desktop/Ardalan_Paper_WTD/Wells_Canada_20220227.csv")
AllUSData4 <- read.csv("C:/Users/amini/Desktop/Ardalan_Paper_WTD/AllUSData4.csv")

Wells_Canada_20220227<-Wells_Canada_20220227[,c("Lat","Lon","Well.depth","WTD..m.")]
colnames(Wells_Canada_20220227)<-c("lat","lon","WellDepth","WTD")
Wells_Canada_20220227<-Wells_Canada_20220227[complete.cases(Wells_Canada_20220227),]
Wells_Canada_20220227$lat<-round(Wells_Canada_20220227$lat,5)
Wells_Canada_20220227$lon<-round(Wells_Canada_20220227$lon,5)


AllUSData4<-AllUSData4[,c("dec_lat_va","dec_long_va","well_depth_va","lev_va")]
AllUSData4$lev_va<-AllUSData4$lev_va*0.3048
AllUSData4$well_depth_va<-AllUSData4$well_depth_va*0.3048
colnames(AllUSData4)<-c("lat","lon","WellDepth","WTD")
AllUSData4<-AllUSData4[complete.cases(AllUSData4),]
AllUSData4<-AllUSData4[AllUSData4$WellDepth<9583,] #largest well in us

AllUSData4$lat<-round(AllUSData4$lat,5)
AllUSData4$lon<-round(AllUSData4$lon,5)

AllUSData4<-aggregate(AllUSData4,by=list(x=AllUSData4$lon,y=AllUSData4$lat,z=AllUSData4$WellDepth),FUN=min)
AllUSData4$x<-NULL
AllUSData4$y<-NULL
AllUSData4$z<-NULL



Wells_tolabel<-rbind(Wells_Canada_20220227,AllUSData4)
rm(AllUSData4,Wells_Canada_20220227)

Wells_tolabel<-Wells_tolabel[complete.cases(Wells_tolabel),]

Wells_tolabel<-Wells_tolabel[Wells_tolabel$WellDepth>=0,] #only use valid well depths


#get climate data
bigRas<-raster("C:/Users/amini/Desktop/Ardalan_Paper_WTD/Rasters/AI_Terra_EMDNA.tif")
pointCoordinates<-Wells_tolabel[,2:1]
coordinates(pointCoordinates)= ~ lon+ lat
rasValue=extract(bigRas, pointCoordinates)

Wells_tolabel$AI<-rasValue
rm(bigRas,pointCoordinates,rasValue)

Wells_tolabel<-Wells_tolabel[complete.cases(Wells_tolabel),]


#import training data
USGS_Wells_20220216 <- read.csv("C:/Users/amini/Desktop/Ardalan_Paper_WTD/USGS_Wells_20220216.csv")
Province_Conf_Unconf <- read_excel("C:/Users/amini/Desktop/Ardalan_Paper_WTD/Province_Conf_Unconf.xlsx")

USGS_Wells_20220216<-USGS_Wells_20220216[USGS_Wells_20220216$Confined.U!=5,]
USGS_Wells_20220216$Confined.U[USGS_Wells_20220216$Confined.U==2]<-1
USGS_Wells_20220216$Confined.U[USGS_Wells_20220216$Confined.U %in% c(3,4)]<-0

colnames(USGS_Wells_20220216)<-c("WTD","Confined","WellDepth","DTB","Slope","Elevation","AI","lat","lon")
USGS_Wells_20220216$DTB<-NULL
USGS_Wells_20220216$Slope<-NULL
USGS_Wells_20220216$Elevation<-NULL
USGS_Wells_20220216$AI<-NULL

colnames(Province_Conf_Unconf)<-c("lat","lon","WellDepth","WTD","remove","Confined")
Province_Conf_Unconf$remove<-NULL
Province_Conf_Unconf<-Province_Conf_Unconf[,c(4,5,3,1,2)]
Province_Conf_Unconf<-Province_Conf_Unconf[complete.cases(Province_Conf_Unconf),]
Province_Conf_Unconf$lat<-round(Province_Conf_Unconf$lat,5)
Province_Conf_Unconf$lon<-round(Province_Conf_Unconf$lon,5)


USGS_Wells_20220216$country<-"US"
Province_Conf_Unconf$country<-"Canada"
ALL<-rbind(USGS_Wells_20220216,Province_Conf_Unconf)
rm(USGS_Wells_20220216,Province_Conf_Unconf)

ALL<-ALL[complete.cases(ALL),]

#add AI to training data
bigRas<-raster("C:/Users/amini/Desktop/Ardalan_Paper_WTD/Rasters/AI_Terra_EMDNA.tif")
pointCoordinates<-ALL[,c("lon","lat")]
coordinates(pointCoordinates)= ~ lon+ lat
rasValue=extract(bigRas, pointCoordinates)
ALL$AI<-rasValue
rm(rasValue,bigRas,pointCoordinates)

ALL<-ALL[complete.cases(ALL),]

gotree<-evtree(Confined~AI+WellDepth,
               data = ALL,control = evtree.control(maxdepth = 2,
                                                                   niterations = 2000, 
                                                                   ntrees = 150))

gotree[[1]]

#evaluation on training set
predictions<-as.numeric(predict(gotree,ALL))
thresh<-.5
predictions[predictions<=thresh]<-0
predictions[predictions>thresh]<-1
sum(predictions==1 & ALL$Confined==1)
sum(predictions==0 & ALL$Confined==0)
sum(predictions==0 & ALL$Confined==1)
sum(predictions==1 & ALL$Confined==0)
sum(predictions== ALL$Confined)/nrow(ALL)
sum(predictions==0 & ALL$Confined==0)/sum(predictions==0)
table(ALL$Confined,ALL$country)


Wells_tolabel$Confined<-as.numeric(predict(gotree,Wells_tolabel))
Wells_tolabel$Confined[Wells_tolabel$Confined<=thresh]<-0
Wells_tolabel$Confined[Wells_tolabel$Confined>thresh]<-1
table(Wells_tolabel$Confined)

True<-ALL[,c("lat","lon","Confined")]
colnames(True)<-c("lat","lon","TrueLabels")

True<-merge(Wells_tolabel,True,by=c("lat","lon"),all.x=T)

what<-duplicated(True[,c("lat","lon")])



#compare predictions with true labels again after merge
mean(True$Confined[!is.na(True$TrueLabels)]==True$TrueLabels[!is.na(True$TrueLabels)])
sum(!is.na(True$TrueLabels))

True$FinalLabels<-True$Confined
True$FinalLabels[!is.na(True$TrueLabels)]<-True$TrueLabels[!is.na(True$TrueLabels)]

write.csv(True,"C:/Users/amini/Desktop/Ardalan_Paper_WTD/ConfinedClassification/Combined2.csv",row.names = F)





