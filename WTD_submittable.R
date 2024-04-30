########################################################################################################################################

#=====Loading Raster Data
rm(list = ls())
#######################################################################################################################################
library(raster)
files<-list.files("C:/Users/amini/Desktop/Ardalan_Paper_WTD/Rasters",full.names = T)
bigRas<-raster(files[1])
for(i in 2:length(files)){
  rCur<-raster(files[i])
  bigRas<-stack(bigRas,rCur)
}
rm(rCur,files,i)

nrows<-dim(bigRas)[1]


#t<-Sys.time()
#what<-getValues(bigRas,row=1)
#cells <- cellFromRow(bigRas,1)
#vals<-extract(bigRas, cells)
#Sys.time()-t

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
  dat<-dat[complete.cases(dat[,c("AI_Terra_EMDNA_Ext","Precipitation_1979_2009_DS_Ext","Prec_Evap_Ext")]),]
  if(nrow(dat)>0){
    dimnames(dat)<-NULL
    saveRDS(dat,file = paste0("C:/Users/amini/Desktop/Ardalan_Paper_WTD/RasterRows/Row",i))
  }
}
stopCluster(cl)

#######################################################################################
##################################  Save dataframe (V1) #################################
########################################################################################
rm(list=ls())

library(stringr)
library(data.table)
files<-list.files("C:/Users/amini/Desktop/Ardalan_Paper_WTD/RasterRows",full.names = T)
rasters<-list.files("C:/Users/amini/Desktop/Ardalan_Paper_WTD/Rasters",full.names = F)
names<-c("lon","lat",substr(rasters,start=1,stop = str_length(rasters)-4))
FanLoc<-which(names %in% c("Fan_WTD_Sim","MaxwellET_WTD_Ext"))
dat<-list()
for(i in 1:length(files)){
  curdat<-as.data.frame(readRDS(files[i]))
  colnames(curdat)<-names
  pekelLoc<-rep(F,nrow(curdat))
  #pekelLoc<-(curdat$Pekel_Occurence >= 75 & !is.na(curdat$Pekel_Occurence))
  n<-nrow(curdat)
  handLoc<-rep(F,nrow(curdat))
  #handLoc<- (sample(c(T,F),size = n,prob=c(0.03, 0.97),replace = T) & !is.na(curdat$HAND_MERIT) & curdat$HAND_MERIT >30 )
  
  curdat<-curdat[!is.na(curdat[,"WTD_Unconf_Med_Ext"]) | pekelLoc | handLoc,]
  dat[[i]]<-curdat
  if(i %% 1000 == 1) print(i)
}
dat<-rbindlist(dat)
dat<-as.data.frame(dat)
dat$TI_Marthews2019[dat$TI_Marthews2019< 0]<-20
colnames(dat)[ncol(dat)]<-"WTD"
summary(dat$WTD)
dat<-dat[dat$WTD>= 0 & dat$WTD<= 1000,]
summary(dat$WTD)
dat$DTB[is.na(dat$DTB)]<- -1
dat$LC_Ext[is.na(dat$LC_Ext)]<- -1
dat$Slope_SRTM[is.na(dat$Slope_SRTM)]<-0
dat$HAND_MERIT[is.na(dat$HAND_MERIT)]<-0



#round numbers to sig figs
dat$AI_Terra_EMDNA_Ext<-round(dat$AI_Terra_EMDNA_Ext,2) #change 2-> 1
dat$Snowfrac_1979_2009_DS_Ext<-round(dat$Snowfrac_1979_2009_DS_Ext,2)
dat$SWE_Max_Ext<-round(dat$SWE_Max_Ext,0)
dat$Temp_1979_2009_DS_Ext<-round(dat$Temp_1979_2009_DS_Ext,1)
dat$TempJan_1979_2009_DS_Ext<-round(dat$TempJan_1979_2009_DS_Ext,1)
dat$HAND_MERIT<-round(dat$HAND_MERIT,0) #change 1 -> 0
dat$Precipitation_1979_2009_DS_Ext<-round(dat$Precipitation_1979_2009_DS_Ext,0) #change 1-> 0
dat$RainInt_1979_2009_Ext<-round(dat$RainInt_1979_2009_Ext,1)
dat$Slope_SRTM<-round(dat$Slope_SRTM,2)
dat$DEM_SRTM_CANUS<-round(dat$DEM_SRTM_CANUS,0)
dat$DTB<-round(dat$DTB/100,0) #change 2 -> 0
dat$WTD<-round(dat$WTD,2)
dat$TI_Marthews2019<-round(dat$TI_Marthews2019,1)
dat$Prec_Evap_Ext<-round(dat$Prec_Evap_Ext,0)

dat<-dat[dat$lat<65 & dat$lon > -140,]

saveRDS(dat,file = "C:/Users/amini/Desktop/Ardalan_Paper_WTD/V1dat.Rds")


#######################################################################################
##################################  Save dataframe (V2) #################################
########################################################################################
rm(list=ls())

library(stringr)
library(data.table)
files<-list.files("C:/Users/amini/Desktop/Ardalan_Paper_WTD/RasterRows",full.names = T)
rasters<-list.files("C:/Users/amini/Desktop/Ardalan_Paper_WTD/Rasters",full.names = F)
names<-c("lon","lat",substr(rasters,start=1,stop = str_length(rasters)-4))
FanLoc<-which(names %in% c("Fan_WTD_Sim","MaxwellET_WTD_Ext"))
dat<-list()
for(i in 1:length(files)){
  curdat<-as.data.frame(readRDS(files[i]))
  colnames(curdat)<-names
  pekelLoc<-rep(F,nrow(curdat))
  pekelLoc<-(curdat$Pekel_Occurence >= 75 & !is.na(curdat$Pekel_Occurence))
  n<-nrow(curdat)
  handLoc<-rep(F,nrow(curdat))
  #handLoc<- (sample(c(T,F),size = n,prob=c(0.03, 0.97),replace = T) & !is.na(curdat$HAND_MERIT) & curdat$HAND_MERIT >30 )
  
  curdat<-curdat[!is.na(curdat[,"WTD_Unconf_Med_Ext"]) | pekelLoc | handLoc,]
  curdat$WTD_Unconf_Med_Ext[curdat$Pekel_Occurence >= 75 & !is.na(curdat$Pekel_Occurence)]<-0
  dat[[i]]<-curdat
  if(i %% 1000 == 1) print(i)
}
dat<-rbindlist(dat)
dat<-as.data.frame(dat)
dat$TI_Marthews2019[dat$TI_Marthews2019< 0]<-20
colnames(dat)[ncol(dat)]<-"WTD"
summary(dat$WTD)
dat<-dat[dat$WTD>= 0 & dat$WTD<= 1000,]
summary(dat$WTD)
dat$DTB[is.na(dat$DTB)]<- -1
dat$LC_Ext[is.na(dat$LC_Ext)]<- -1
dat$Slope_SRTM[is.na(dat$Slope_SRTM)]<-0
dat$HAND_MERIT[is.na(dat$HAND_MERIT)]<-0



#round numbers to sig figs
dat$AI_Terra_EMDNA_Ext<-round(dat$AI_Terra_EMDNA_Ext,2) #change 2-> 1
dat$Snowfrac_1979_2009_DS_Ext<-round(dat$Snowfrac_1979_2009_DS_Ext,2)
dat$SWE_Max_Ext<-round(dat$SWE_Max_Ext,0)
dat$Temp_1979_2009_DS_Ext<-round(dat$Temp_1979_2009_DS_Ext,1)
dat$TempJan_1979_2009_DS_Ext<-round(dat$TempJan_1979_2009_DS_Ext,1)
dat$HAND_MERIT<-round(dat$HAND_MERIT,0) #change 1 -> 0
dat$Precipitation_1979_2009_DS_Ext<-round(dat$Precipitation_1979_2009_DS_Ext,0) #change 1-> 0
dat$RainInt_1979_2009_Ext<-round(dat$RainInt_1979_2009_Ext,1)
dat$Slope_SRTM<-round(dat$Slope_SRTM,2)
dat$DEM_SRTM_CANUS<-round(dat$DEM_SRTM_CANUS,0)
dat$DTB<-round(dat$DTB/100,0) #change 2 -> 0
dat$WTD<-round(dat$WTD,2)
dat$TI_Marthews2019<-round(dat$TI_Marthews2019,1)
dat$Prec_Evap_Ext<-round(dat$Prec_Evap_Ext,0)

dat<-dat[dat$lat<65 & dat$lon > -140,]
dat$Pekel_Occurence[is.na(dat$Pekel_Occurence)]<-0

saveRDS(dat,file = "C:/Users/amini/Desktop/Ardalan_Paper_WTD/V2dat.Rds")


#######################################################################################
##################################  Save dataframe (V3) #################################
########################################################################################
rm(list=ls())

library(stringr)
library(data.table)
files<-list.files("C:/Users/amini/Desktop/Ardalan_Paper_WTD/RasterRows",full.names = T)
rasters<-list.files("C:/Users/amini/Desktop/Ardalan_Paper_WTD/Rasters",full.names = F)
names<-c("lon","lat",substr(rasters,start=1,stop = str_length(rasters)-4))
FanLoc<-which(names %in% c("Fan_WTD_Sim","MaxwellET_WTD_Ext"))
dat<-list()
for(i in 1:length(files)){
  curdat<-as.data.frame(readRDS(files[i]))
  colnames(curdat)<-names
  pekelLoc<-rep(F,nrow(curdat))
  pekelLoc<-(curdat$Pekel_Occurence >= 75 & !is.na(curdat$Pekel_Occurence))
  n<-nrow(curdat)
  handLoc<-rep(F,nrow(curdat))
  handLoc<- (sample(c(T,F),size = n,prob=c(0.03, 0.97),replace = T) & !is.na(curdat$HAND_MERIT) & curdat$HAND_MERIT >30 )
  
  curdat<-curdat[!is.na(curdat[,"WTD_Unconf_Med_Ext"]) | pekelLoc | handLoc,]
  curdat$WTD_Unconf_Med_Ext[!is.na(curdat$HAND_MERIT) & curdat$HAND_MERIT >30]<-curdat$HAND_MERIT[!is.na(curdat$HAND_MERIT) & curdat$HAND_MERIT >30]
  curdat$WTD_Unconf_Med_Ext[curdat$Pekel_Occurence >= 75 & !is.na(curdat$Pekel_Occurence)]<-0
  
  
  dat[[i]]<-curdat
  if(i %% 1000 == 1) print(i)
}
dat<-rbindlist(dat)
dat<-as.data.frame(dat)
dat$TI_Marthews2019[dat$TI_Marthews2019< 0]<-20
colnames(dat)[ncol(dat)]<-"WTD"
summary(dat$WTD)
dat<-dat[dat$WTD>= 0 & dat$WTD<= 1000,]
summary(dat$WTD)
dat$DTB[is.na(dat$DTB)]<- -1
dat$LC_Ext[is.na(dat$LC_Ext)]<- -1
dat$Slope_SRTM[is.na(dat$Slope_SRTM)]<-0
dat$HAND_MERIT[is.na(dat$HAND_MERIT)]<-0



#round numbers to sig figs
dat$AI_Terra_EMDNA_Ext<-round(dat$AI_Terra_EMDNA_Ext,2) #change 2-> 1
dat$Snowfrac_1979_2009_DS_Ext<-round(dat$Snowfrac_1979_2009_DS_Ext,2)
dat$SWE_Max_Ext<-round(dat$SWE_Max_Ext,0)
dat$Temp_1979_2009_DS_Ext<-round(dat$Temp_1979_2009_DS_Ext,1)
dat$TempJan_1979_2009_DS_Ext<-round(dat$TempJan_1979_2009_DS_Ext,1)
dat$HAND_MERIT<-round(dat$HAND_MERIT,0) #change 1 -> 0
dat$Precipitation_1979_2009_DS_Ext<-round(dat$Precipitation_1979_2009_DS_Ext,0) #change 1-> 0
dat$RainInt_1979_2009_Ext<-round(dat$RainInt_1979_2009_Ext,1)
dat$Slope_SRTM<-round(dat$Slope_SRTM,2)
dat$DEM_SRTM_CANUS<-round(dat$DEM_SRTM_CANUS,0)
dat$DTB<-round(dat$DTB/100,0) #change 2 -> 0
dat$WTD<-round(dat$WTD,2)
dat$TI_Marthews2019<-round(dat$TI_Marthews2019,1)
dat$Prec_Evap_Ext<-round(dat$Prec_Evap_Ext,0)

dat<-dat[dat$lat<65 & dat$lon > -140,]
dat$Pekel_Occurence[is.na(dat$Pekel_Occurence)]<-0

saveRDS(dat,file = "C:/Users/amini/Desktop/Ardalan_Paper_WTD/V3dat.Rds")


###########################################################################################################################
# ###################################3    xgboost (V1)    #######################################################################
# #########################################################################################################################


library(xgboost)
library(fastDummies)
rm(list = ls())
dat<-readRDS("C:/Users/amini/Desktop/Ardalan_Paper_WTD/V1dat.Rds")





explanatories<-c(3:7,10,13:17,19:26)
obs_wei<-obs_wei[dat$lat<65 & dat$lon > -140]
dat<-dat[dat$lat<65 & dat$lon > -140,]
sum(dat$Pekel_Occurence>= 75 & !is.na(dat$Pekel_Occurence))
nrow(dat)

# get training, validation, test set
set<-rep("",nrow(dat))
testreg<-(!is.na(dat$MaxwellET_WTD_Ext))
#testreg<-(dat$lon> -115 & dat$lat<48 & dat$lon< -80 & dat$lat>35)
set.seed(123)
set[testreg]<-sample(c("train","test","val"),size = sum(testreg),replace = T)
set.seed(123)
set[!testreg]<-sample(c("train","train","val"),size = sum(!testreg),replace = T)
rm(testreg)


#choose hyperparameters
get_monotone<-function(cnames){
  OUTPUT<-rep(0,length(cnames))
  OUTPUT[cnames %in% c("sand_0_5cm_Ext","sand_100_200cm_Ext","AI_Terra_EMDNA_Ext")]<-1
  OUTPUT[cnames %in% c("Prec_Evap_Ext","TI_Marthews2019","clay_100_200cm_Ext","clay_0_5cm_Ext","Precipitation_1979_2009_DS_Ext")]<- -1
  OUTPUT
}

numr<-20
set.seed(123)
HPOdat<-data.frame(eta=runif(numr,min=1e-4, max=.15),ss=runif(numr,min=.5,max=1),md=sample(10:15,numr,replace = T),
                   cs=runif(numr,min=.25,max=1),nt=sample(1000,numr,replace = T),mcw=sample(1:50,numr,replace = T),gam=runif(numr,0,100)
                   ,lam=runif(numr,0,100),alp=runif(numr,0,100),r2=rep(NA,numr))


for(i in which(is.na(HPOdat$r2))){
  set.seed(123)
  hp_list<-list(eta=HPOdat$eta[i],subsample=HPOdat$ss[i],max_depth=HPOdat$md[i],min_child_weight=HPOdat$mcw[i],
                gamma=HPOdat$gam[i], lambda=HPOdat$lam[i],alpha=HPOdat$alp[i], 
                colsample_bytree=HPOdat$cs[i],monotone_constraints= get_monotone(colnames(dat)[explanatories]))
  mod<-xgboost(params=hp_list,data=as.matrix(dat[set=="train",explanatories]),
                                     label=dat$WTD[set=="train"],nrounds=HPOdat$nt[i],verbose = 0,weight=obs_wei[set=="train"])
  pred20<-predict(mod,xgb.DMatrix(as.matrix(dat[set=="val",explanatories])))

  
  pred20[pred20<0]<-0
  HPOdat$r2[i]<-1-sum((pred20-dat$WTD[set=="val"])^2)/sum((mean(dat$WTD[set=="val"])-dat$WTD[set=="val"])^2)
  print(HPOdat$r2[i])
  write.csv(HPOdat,"C:/Users/amini/Desktop/Ardalan_Paper_WTD/Results/HPOdat.csv",row.names=F)
}
HPOdat<-read.csv("C:/Users/amini/Desktop/Ardalan_Paper_WTD/Results/HPOdat.csv")
b<-which.max(HPOdat$r2)

#test set evaluation
set.seed(123)
mod<-xgboost(params=list(eta=HPOdat$eta[b],subsample=HPOdat$ss[b],max_depth=HPOdat$md[b],min_child_weight=HPOdat$mcw[b],gamma=HPOdat$gam[b],
                    lambda=HPOdat$lam[b],alpha=HPOdat$alp[b], colsample_bytree=HPOdat$cs[b]),monotone_constraints= get_monotone(colnames(dat)[explanatories]),
               data=as.matrix(dat[set=="train",explanatories]),label=dat$WTD[set=="train"],nrounds=HPOdat$nt[b],verbose = 0,weight=obs_wei[set=="train"])
pred20<-predict(mod,xgb.DMatrix(as.matrix(dat[set=="test",explanatories])))
pred20[pred20<0]<-0
xgb.importance(colnames(dat[,explanatories]),mod)
1-sum((pred20-dat$WTD[set=="test"])^2)/sum((mean(dat$WTD[set=="test"])-dat$WTD[set=="test"])^2)
sqrt(mean((pred20-dat$WTD[set=="test"])^2))
cor(pred20,dat$WTD[set=="test"])
mean(abs(pred20-dat$WTD[set=="test"])<1)
mean(abs(pred20-dat$WTD[set=="test"])<2)
mean(abs(pred20-dat$WTD[set=="test"])<5)
mean(abs(pred20-dat$WTD[set=="test"])<10)
mean(abs(pred20-dat$WTD[set=="test"])<20)

originalobs_flag<- (dat$Pekel_Occurence[set=="test"]<75 | is.na(dat$Pekel_Occurence[set=="test"])) & round(dat$WTD[set=="test"])!=dat$HAND_MERIT[set=="test"]
1-sum((pred20[originalobs_flag]-dat$WTD[set=="test"][originalobs_flag])^2)/sum((mean(dat$WTD[set=="test"][originalobs_flag])-dat$WTD[set=="test"][originalobs_flag])^2)

jpeg("C:/Users/amini/Desktop/Ardalan_Paper_WTD/Results/Plots/WTDtestsetML.jpg",width = 5, height = 5, units = 'in',res = 500)
plot(pred20,dat$WTD[set=="test"],xlab="Simulated WTD (m)",ylab="Observed WTD (m)",pch=20,cex=.5,cex.lab=1)
lines(0:200,0:200,lwd=2,col="red")
dev.off()



#calculate Fan RMSE and R^2
dat$Fan_WTD_Sim[dat$Fan_WTD_Sim>200]<-200
1-sum((dat$Fan_WTD_Sim[set=="test"]-dat$WTD[set=="test"])^2)/sum((mean(dat$WTD[set=="test"])-dat$WTD[set=="test"])^2)
sqrt(mean((dat$Fan_WTD_Sim[set=="test"]-dat$WTD[set=="test"])^2))
mean(abs(dat$Fan_WTD_Sim[set=="test"]-dat$WTD[set=="test"])<1)
mean(abs(dat$Fan_WTD_Sim[set=="test"]-dat$WTD[set=="test"])<2)
mean(abs(dat$Fan_WTD_Sim[set=="test"]-dat$WTD[set=="test"])<5)
mean(abs(dat$Fan_WTD_Sim[set=="test"]-dat$WTD[set=="test"])<10)
mean(abs(dat$Fan_WTD_Sim[set=="test"]-dat$WTD[set=="test"])<20)
cor(dat$Fan_WTD_Sim[set=="test"],dat$WTD[set=="test"])


jpeg("C:/Users/amini/Desktop/Ardalan_Paper_WTD/Results/Plots/WTDtestsetFan.jpg",width = 5, height = 5, units = 'in',res = 500)
plot(dat$Fan_WTD_Sim[set=="test"],dat$WTD[set=="test"],xlab="Simulated WTD (m)",ylab="Observed WTD (m)",pch=20,cex=.5,cex.lab=1)
lines(0:200,0:200,lwd=2,col="red")
dev.off()


#calculate Maxwell RMSE and R^2
dat$MaxwellET_WTD_Ext[dat$MaxwellET_WTD_Ext>200]<-200
dat$MaxwellET_WTD_Ext[dat$MaxwellET_WTD_Ext<0]<-0
1-sum((dat$MaxwellET_WTD_Ext[set=="test"]-dat$WTD[set=="test"])^2)/sum((mean(dat$WTD[set=="test"])-dat$WTD[set=="test"])^2)
sqrt(mean((dat$MaxwellET_WTD_Ext[set=="test"]-dat$WTD[set=="test"])^2))
mean(abs(dat$MaxwellET_WTD_Ext[set=="test"]-dat$WTD[set=="test"])<1)
mean(abs(dat$MaxwellET_WTD_Ext[set=="test"]-dat$WTD[set=="test"])<2)
mean(abs(dat$MaxwellET_WTD_Ext[set=="test"]-dat$WTD[set=="test"])<5)
mean(abs(dat$MaxwellET_WTD_Ext[set=="test"]-dat$WTD[set=="test"])<10)
mean(abs(dat$MaxwellET_WTD_Ext[set=="test"]-dat$WTD[set=="test"])<20)
cor(dat$MaxwellET_WTD_Ext[set=="test"],dat$WTD[set=="test"])


jpeg("C:/Users/amini/Desktop/Ardalan_Paper_WTD/Results/Plots/WTDtestsetFan.jpg",width = 5, height = 5, units = 'in',res = 500)
plot(dat$Fan_WTD_Sim[set=="test"],dat$WTD[set=="test"],xlab="Simulated WTD (m)",ylab="Observed WTD (m)",pch=20,cex=.5,cex.lab=1)
lines(0:200,0:200,lwd=2,col="red")
dev.off()





#train full model
set.seed(123)
mod<-xgboost(params=list(eta=HPOdat$eta[b],subsample=HPOdat$ss[b],max_depth=HPOdat$md[b],min_child_weight=HPOdat$mcw[b],
                    colsample_bytree=HPOdat$cs[b],gamma=HPOdat$gam[b],lambda=HPOdat$lam[b],alpha=HPOdat$alp[b],
                    monotone_constraints= get_monotone(colnames(dat)[explanatories]))
               ,data=as.matrix(dat[,explanatories]),label=dat$WTD,nrounds=HPOdat$nt[b],verbose = 0,weight=obs_wei)
predFull<-predict(mod,xgb.DMatrix(as.matrix(dat[,explanatories])))
predFull[predFull<0]<-0
predFull[predFull>1000]<-1000
xgb.importance(colnames(dat[,explanatories]),mod)

1-sum((predFull[set=="test"]-dat$WTD[set=="test"])^2)/sum((mean(dat$WTD[set=="test"])-dat$WTD[set=="test"])^2)
sqrt(mean((predFull[set=="test"]- dat$WTD[set=="test"])^2))
mean(abs(predFull[set=="test"]-dat$WTD[set=="test"])<1)
mean(abs(predFull[set=="test"]-dat$WTD[set=="test"])<2)
mean(abs(predFull[set=="test"]-dat$WTD[set=="test"])<5)
mean(abs(predFull[set=="test"]-dat$WTD[set=="test"])<10)
mean(abs(predFull[set=="test"]-dat$WTD[set=="test"])<20)

originalobs_flag<- (dat$Pekel_Occurence[set=="test"]<75 | is.na(dat$Pekel_Occurence[set=="test"])) & obs_wei[set=="test"]==1
1-sum((predFull[set=="test"][originalobs_flag]-dat$WTD[set=="test"][originalobs_flag])^2)/sum((mean(dat$WTD[set=="test"][originalobs_flag])-dat$WTD[set=="test"][originalobs_flag])^2)




jpeg("C:/Users/amini/Desktop/Ardalan_Paper_WTD/Results/Plots/WTDModelFitML.jpg",width = 5, height = 5, units = 'in',res = 500)
plot(predFull,dat$WTD,xlab="Simulated WTD (m)",ylab="Observed WTD (m)",pch=20,cex=.5,cex.lab=1)
lines(0:200,0:200,lwd=2,col="red")
dev.off()

#save weird observations
weirdobs<-dat[abs(predFull-dat$WTD)>30,]
weirdobs$pred<-predFull[abs(predFull-dat$WTD)>30]
write.csv(weirdobs,"C:/Users/amini/Desktop/Ardalan_Paper_WTD/Results/WeirdObsUnconfMed.csv",row.names = F)
rm(weirdobs)


#make predictions
dir.create("C:/Users/amini/Desktop/Ardalan_Paper_WTD/RasterRowsWpred")

library(stringr)
library(data.table)
files<-list.files("C:/Users/amini/Desktop/Ardalan_Paper_WTD/RasterRows",full.names = T)
rasters<-list.files("C:/Users/amini/Desktop/Ardalan_Paper_WTD/Rasters",full.names = F)
names<-c("lon","lat",substr(rasters,start=1,stop = str_length(rasters)-4))
for(i in 1:length(files)){
  curdat<-readRDS(files[i])
  if(nrow(curdat)>0){
    colnames(curdat)<-names
    dat<-as.data.frame(curdat)
    dat$DTB[is.na(dat$DTB)]<- -1
    dat$LC_Ext[is.na(dat$LC_Ext)]<- -1
    dat$Slope_SRTM[is.na(dat$Slope_SRTM)]<-0
    dat$HAND_MERIT[is.na(dat$HAND_MERIT)]<-0
    dat$TI_Marthews2019[dat$TI_Marthews2019< 0]<-20
    dat$DTB<-dat$DTB/100
    
    pred<-predict(mod,xgb.DMatrix(as.matrix(dat[,explanatories])))
    d<-cbind(dat,pred)
    saveRDS(d,file = paste0("C:/Users/amini/Desktop/Ardalan_Paper_WTD/RasterRowsWpred/Row",i))
  }
}


rm(list = ls())
gc()
files<-list.files('C:/Users/amini/Desktop/Ardalan_Paper_WTD/RasterRowsWpred',full.names = T)
Pred<-list()
for(i in 1:length(files)){
  curdat<-as.data.frame(readRDS(files[i]))
  Pred[[i]]<-curdat[,c("lon","lat","pred")]
  if(i %% 1000 ==1) print(i)
}
rm(curdat)

library(data.table)
library(raster)
PredMat = rbindlist(Pred)
rm(Pred)
gc()
PredMat$pred[PredMat$pred<0]<-0
PredMat$pred[PredMat$pred>1000]<-1000
a<-rasterFromXYZ(PredMat)
rm(PredMat)
gc()
writeRaster(a, "C:/Users/amini/Desktop/Ardalan_Paper_WTD/Results/pred_final_V3.tif",overwrite=T) 


#check raster results
rm(list = ls())
dat<-readRDS("C:/Users/amini/Desktop/Ardalan_Paper_WTD/dat2.Rds")

#round numbers to sig figs
dat$AI_Terra_EMDNA_Ext<-round(dat$AI_Terra_EMDNA_Ext,2) #change 2-> 1
dat$Snowfrac_1979_2009_DS_Ext<-round(dat$Snowfrac_1979_2009_DS_Ext,2)
dat$SWE_Max_Ext<-round(dat$SWE_Max_Ext,0)
dat$Temp_1979_2009_DS_Ext<-round(dat$Temp_1979_2009_DS_Ext,1)
dat$TempJan_1979_2009_DS_Ext<-round(dat$TempJan_1979_2009_DS_Ext,1)
dat$HAND_MERIT<-round(dat$HAND_MERIT,0) #change 1 -> 0
dat$Precipitation_1979_2009_DS_Ext<-round(dat$Precipitation_1979_2009_DS_Ext,0) #change 1-> 0
dat$RainInt_1979_2009_Ext<-round(dat$RainInt_1979_2009_Ext,1)
dat$Slope_SRTM<-round(dat$Slope_SRTM,2)
dat$DEM_SRTM_CANUS<-round(dat$DEM_SRTM_CANUS,0)
dat$DTB<-round(dat$DTB/100,0) #change 2 -> 0
dat$WTD<-round(dat$WTD,2)
dat$TI_Marthews2019<-round(dat$TI_Marthews2019,1)
dat$Prec_Evap_Ext<-round(dat$Prec_Evap_Ext,0)

dat$AISlope<-dat$AI_Terra_EMDNA_Ext*dat$Slope_SRTM
dat$AIHand<-dat$AI_Terra_EMDNA_Ext*dat$HAND_MERIT


explanatories<-c(3:7,9:11,14:26,28:29)

get_monotone<-function(cnames){
  OUTPUT<-rep(0,length(cnames))
  OUTPUT[cnames %in% c("AI_Terra_EMDNA_Ext","Prec_Evap_Ext","Temp1979_2009_DS_Ext","TempJan1979_2009_DS_Ext","AISlope","AIHand","sand_0_5cm_Ext","sand_100_200cm_Ext")]<-1
  OUTPUT[cnames %in% c("Precipitation_1979_2009_DS_Ext","clay_0_5cm_Ext","clay_100_200cm_Ext","Snowfrac_1979_2009_DS_Ext","SWE_Max_Ext")]<- -1
  OUTPUT
}


HPOdat<-read.csv("C:/Users/amini/Desktop/Ardalan_Paper_WTD/Results/HPOdat.csv")
b<-which.max(HPOdat$r2)

set.seed(123)
mod<-xgb.train(list(eta=HPOdat$eta[b],subsample=HPOdat$ss[b],max_depth=HPOdat$md[b],min_child_weight=HPOdat$mcw[b],
                    colsample_bytree=HPOdat$cs[b],gamma=HPOdat$gam[b],lambda=HPOdat$lam[b],alpha=HPOdat$alp[b],
                    monotone_constraints= get_monotone(colnames(dat)[explanatories]))
               ,xgb.DMatrix(as.matrix(dat[,explanatories]),label=dat$WTD),nrounds=HPOdat$nt[b],verbose = 0)
predFull<-predict(mod,xgb.DMatrix(as.matrix(dat[,explanatories])))
predFull[predFull<0]<-0
predFull[predFull>200]<-200

rCur<-raster("C:/Users/amini/Desktop/Ardalan_Paper_WTD/Results/pred_unconfMED_V14_ne1_20m_20km_mono.tif")
result <- extract(rCur, dat[,1:2])
cor(result,predFull)
cor(result,dat$WTD)

#compare wtd with hydrological signatures
rm(list = ls())
gc()
library(sp)
a<-raster("C:/Users/amini/Desktop/Ardalan_Paper_WTD/Results/pred_unconfMED_V12_ne1_20m_20km_mono.tif")
shp <- shapefile('D:/Data/CANUS Boundary Files/CANUS_Watershed_Boundaries.shp')
wtdCatchment <- extract(a, shp)

r.mean <- lapply(wtdCatchment, FUN=mean,na.rm=T)
r.sd <- lapply(wtdCatchment, FUN=sd,na.rm=T)
wtdVShs<-data.frame(gridcode=as.numeric(shp$gridcode),lon=shp$Longitude,lat=shp$Latitude,area=shp$Final_Area,wtdMean=as.numeric(r.mean),wtdSD=as.numeric(r.sd))




library(readxl)
overall_hydrological_signatures2 <- read_excel("C:/Users/amini/Desktop/Ardalan_Paper_WTD/overall_hydrological_signatures2.xlsx", 
                                               col_types = c("numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                   "numeric", "numeric", "numeric", 
                                                                   "numeric", "numeric", "numeric", 
                                                                   "numeric", "numeric", "numeric", 
                                                                   "numeric", "numeric", "numeric"))

yearAvails_2020 <- read_excel("D:/Data/Clustering/yearAvails_2020.xlsx", col_types = 
                                c("numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

wtdVShs2<-merge(wtdVShs,overall_hydrological_signatures2,by="gridcode",all.x=T)
wtdVShs2$...16<-NULL
wtdVShs2$...17<-NULL
wtdVShs2$rel_error<-NA
for(i in 1:nrow(wtdVShs2)){
  gc<-wtdVShs2$gridcode[i]
  wtdVShs2$rel_error[i]<-as.numeric(yearAvails_2020$Rel_Error[yearAvails_2020$gridcode==gc])
}

EMDNA_P<-read.csv("D:/Data/EMDNA_GRIDCODE/prcp.csv")
EMDNA_T<-read.csv("D:/Data/EMDNA_GRIDCODE/tmean.csv")
PETyearly<-read.csv("D:/Data/Clustering/TerraETyearly.csv")
ERA5L_P<-read.csv("D:/ERA5_Land_Yearly_GRID/TotalPrecipYear.csv")
tt<-tp<-365.25
wtdVShs2$SI<-wtdVShs2$AI<-NA
library(lubridate)
dates<-seq(from=as.Date("1979-01-01"),to=as.Date("2018-12-31"),by="day")
for(i in 1:nrow(wtdVShs2)){
  if(sum(EMDNA_P$Allresults==wtdVShs2$gridcode[i])>0){
    y=as.numeric(EMDNA_P[EMDNA_P$Allresults==wtdVShs2$gridcode[i],2:ncol(EMDNA_P)])
    t<-1:length(y)
    y=(y/mean(y))-1
    scurve<-sin(2*pi*t/tp)
    ccurve<-cos(2*pi*t/tp)
    linmod<-lm(y~scurve+ccurve)
    sp=tp*atan(-coef(linmod)[3]/coef(linmod)[2])/(2*pi)
    deltap=coef(linmod)[2]/cos(2*pi*sp/tp)
    y=as.numeric(EMDNA_T[EMDNA_T$Allresults==wtdVShs2$gridcode[i],2:ncol(EMDNA_T)])
    y=y-mean(y)
    scurve<-sin(2*pi*t/tt)
    ccurve<-cos(2*pi*t/tt)
    linmod<-lm(y~scurve+ccurve)
    st=tt*atan(-coef(linmod)[3]/coef(linmod)[2])/(2*pi)
    deltat=coef(linmod)[2]/cos(2*pi*st/tt)
    seasonalityIndex<-as.numeric(deltap*sign(deltat)*cos(2*pi*(sp-st)/tt))
    wtdVShs2$SI[i]<-seasonalityIndex
    
    PETvec<-as.numeric(PETyearly[PETyearly$V1==wtdVShs2$gridcode[i],2:ncol(PETyearly)])
    Pvec<-rep(0,length(PETvec))
    Yvec<-1981:2018
    
    y=as.numeric(EMDNA_P[EMDNA_P$Allresults==wtdVShs2$gridcode[i],2:ncol(EMDNA_P)])
    for(j in 1:length(Pvec)){
      Pvec[j]<-sum(y[year(dates)==Yvec[j]])
    }
    
    wtdVShs2$AI[i]<-mean(PETvec/Pvec)
  }
}

write.csv(wtdVShs2, "C:/Users/amini/Desktop/Ardalan_Paper_WTD/Results/wtdVShs2.csv",row.names = F) 


ClusteringDataNonTransformedFinal4 <- read.csv("D:/Data/Clustering/ClusteringDataNonTransformedFinal4.csv")

full<-merge(ClusteringDataNonTransformedFinal4,wtdVShs2[,c("gridcode","wtdMean")],all.x=T,by="gridcode")


rfmod<-ranger(x=full[,c("SeasonalityWI","PExceed","porespace_P","SnowFrac","AET_P","Bedrock")], y=full$bfi,oob.error = T,verbose = F,num.trees = 900)
rfmod$r.squared

rfmod<-ranger(x=full[,c("SeasonalityWI","PExceed","porespace_P","SnowFrac","AET_P","Bedrock","wtdMean")], y=full$bfi,oob.error = T,verbose = F,num.trees = 900)
rfmod$r.squared





#test on mexico data
library(readxl)
MexicoData <- read_excel("C:/Users/amini/Desktop/Ardalan_Paper_WTD/MexicoData.xlsx")



bigRas<-raster("C:/Users/amini/Desktop/Ardalan_Paper_WTD/Results/pred_unconfMED_V34_ne1_20m_20km_mono.tif")
pointCoordinates<-MexicoData[,c("Lon","Lat")]
coordinates(pointCoordinates)= ~ Lon+ Lat
rasValue=extract(bigRas, pointCoordinates)
MexicoData$pred<-rasValue

dat<-data.frame(ID=unique(MexicoData$StnID),level=NA,pred=NA)
for(i in 1:nrow(dat)){
  curmeas<-MexicoData[MexicoData$StnID==dat$ID[i],]
  dat$level[i]<-min(curmeas$WaterLevel)
  dat$pred[i]<-min(curmeas$pred)
  
}
dat<-dat[complete.cases(dat),]

cor(dat$pred[dat$level<200],dat$level[dat$level<200])
plot(dat$pred[dat$level<200],dat$level[dat$level<200])

1-sum((dat$level-dat$pred)^2)/sum((dat$level-mean(dat$level))^2)




#################################################################################################################
#######################      run cross validation on each region (V1)    ##############################################3
###############################################################################################################
rm(list = ls())
library(raster)
library(xgboost)
rCur<-raster("C:/Users/amini/Desktop/Ardalan_Paper_WTD/Eco_Regions/EcoRegion_Lev1_Ext.tif") # get raster
dat<-readRDS("C:/Users/amini/Desktop/Ardalan_Paper_WTD/V1dat.Rds") #get final data


explanatories<-c(3:7,10,13:17,19:26)


#choose hyperparameters
get_monotone<-function(cnames){
  OUTPUT<-rep(0,length(cnames))
  OUTPUT[cnames %in% c("sand_0_5cm_Ext","sand_100_200cm_Ext","AI_Terra_EMDNA_Ext")]<-1
  OUTPUT[cnames %in% c("Prec_Evap_Ext","TI_Marthews2019","clay_100_200cm_Ext","clay_0_5cm_Ext","Precipitation_1979_2009_DS_Ext")]<- -1
  OUTPUT
}

HPOdat<-read.csv("C:/Users/amini/Desktop/Ardalan_Paper_WTD/Results/HPOdat.csv")
b<-which.max(HPOdat$r2)

result <- extract(rCur, dat[,1:2])
dat$EcoRegion<-result
rm(result)

what<-as.data.frame(table(dat$EcoRegion))
what<-what[what$Freq>100,]

what$R2<-0
what$corr<-0
what$MAE<-0
for(i in 1:nrow(what)){
  allothers<-dat[dat$EcoRegion!=what$Var1[i] | is.na(dat$EcoRegion),]
  curEcoData<-dat[dat$EcoRegion==what$Var1[i] & !is.na(dat$EcoRegion),]
  folds<-sample(1:10,nrow(curEcoData),replace = T)
  for(j in 1:10){
    curEcoTest<-curEcoData[folds==j,]
    curEcoTrain<-curEcoData[folds!=j,]
    FullTrain<-rbind(allothers,curEcoTrain)
    
    set.seed(123)
    mod<-xgboost(params=list(eta=HPOdat$eta[b],subsample=HPOdat$ss[b],max_depth=HPOdat$md[b],min_child_weight=HPOdat$mcw[b],gamma=HPOdat$gam[b],
                             lambda=HPOdat$lam[b],alpha=HPOdat$alp[b], colsample_bytree=HPOdat$cs[b]),monotone_constraints= get_monotone(colnames(dat)[explanatories]),
                 data=xgb.DMatrix(as.matrix(FullTrain[,explanatories]),label=FullTrain$WTD),nrounds=HPOdat$nt[b],verbose = 0)
    pred20<-predict(mod,xgb.DMatrix(as.matrix(curEcoTest[,explanatories])))
    pred20[pred20<0]<-0
    what$R2[i]<-what$R2[i]+ 1-sum((pred20- curEcoTest$WTD)^2)/sum((mean(curEcoTest$WTD)-curEcoTest$WTD)^2)
    what$corr[i]<- what$corr[i] + cor(pred20,curEcoTest$WTD)
    what$MAE[i]<-what$MAE[i] + mean(abs(pred20 - curEcoTest$WTD))
  }
  print(i)
  print(what$R2[i])
}
what$R2<-what$R2/10
what$corr<-what$corr/10
what$MAE<-what$MAE/10


write.csv(what, "C:/Users/amini/Desktop/Ardalan_Paper_WTD/Results/V1_cv_results.csv",row.names = F)



R2<-0
corr<-0
MAE<-0
folds<-sample(1:10,nrow(dat),replace = T)
for(j in 1:10){
  curEcoTest<-dat[folds==j,]
  FullTrain<-dat[folds!=j,]
  
  set.seed(123)
  mod<-xgboost(params=list(eta=HPOdat$eta[b],subsample=HPOdat$ss[b],max_depth=HPOdat$md[b],min_child_weight=HPOdat$mcw[b],gamma=HPOdat$gam[b],
                           lambda=HPOdat$lam[b],alpha=HPOdat$alp[b], colsample_bytree=HPOdat$cs[b]),monotone_constraints= get_monotone(colnames(dat)[explanatories]),
               data=xgb.DMatrix(as.matrix(FullTrain[,explanatories]),label=FullTrain$WTD),nrounds=HPOdat$nt[b],verbose = 0)
  pred20<-predict(mod,xgb.DMatrix(as.matrix(curEcoTest[,explanatories])))
  pred20[pred20<0]<-0
  R2<-R2+ 1-sum((pred20- curEcoTest$WTD)^2)/sum((mean(curEcoTest$WTD)-curEcoTest$WTD)^2)
  corr<- corr + cor(pred20,curEcoTest$WTD)
  MAE<-MAE + mean(abs(pred20 - curEcoTest$WTD))
}
R2/10
corr/10
MAE/10


#################################################################################################################
#######################      run cross validation on each region (V2)    ##############################################3
###############################################################################################################
rm(list = ls())
library(raster)
library(xgboost)
rCur<-raster("C:/Users/amini/Desktop/Ardalan_Paper_WTD/Eco_Regions/EcoRegion_Lev1_Ext.tif") # get raster
dat<-readRDS("C:/Users/amini/Desktop/Ardalan_Paper_WTD/V2dat.Rds") #get final data


explanatories<-c(3:7,10,13:17,19:26)


#choose hyperparameters
get_monotone<-function(cnames){
  OUTPUT<-rep(0,length(cnames))
  OUTPUT[cnames %in% c("sand_0_5cm_Ext","sand_100_200cm_Ext","AI_Terra_EMDNA_Ext")]<-1
  OUTPUT[cnames %in% c("Prec_Evap_Ext","TI_Marthews2019","clay_100_200cm_Ext","clay_0_5cm_Ext","Precipitation_1979_2009_DS_Ext")]<- -1
  OUTPUT
}

HPOdat<-read.csv("C:/Users/amini/Desktop/Ardalan_Paper_WTD/Results/HPOdat.csv")
b<-which.max(HPOdat$r2)

result <- extract(rCur, dat[,1:2])
dat$EcoRegion<-result
rm(result)

what<-as.data.frame(table(dat$EcoRegion))
what<-what[what$Freq>100,]

what$R2<-0
what$corr<-0
what$MAE<-0
for(i in 1:nrow(what)){
  allothers<-dat[dat$EcoRegion!=what$Var1[i] | is.na(dat$EcoRegion) | dat$Pekel_Occurence>=75,]
  curEcoData<-dat[dat$EcoRegion==what$Var1[i] & !is.na(dat$EcoRegion) & dat$Pekel_Occurence<75,]
  if(nrow(curEcoData)>90){
    folds<-sample(1:10,nrow(curEcoData),replace = T)
    for(j in 1:10){
      curEcoTest<-curEcoData[folds==j,]
      curEcoTrain<-curEcoData[folds!=j,]
      FullTrain<-rbind(allothers,curEcoTrain)
      
      set.seed(123)
      mod<-xgboost(params=list(eta=HPOdat$eta[b],subsample=HPOdat$ss[b],max_depth=HPOdat$md[b],min_child_weight=HPOdat$mcw[b],gamma=HPOdat$gam[b],
                               lambda=HPOdat$lam[b],alpha=HPOdat$alp[b], colsample_bytree=HPOdat$cs[b]),monotone_constraints= get_monotone(colnames(dat)[explanatories]),
                   data=xgb.DMatrix(as.matrix(FullTrain[,explanatories]),label=FullTrain$WTD),nrounds=HPOdat$nt[b],verbose = 0)
      pred20<-predict(mod,xgb.DMatrix(as.matrix(curEcoTest[,explanatories])))
      pred20[pred20<0]<-0
      what$R2[i]<-what$R2[i]+ 1-sum((pred20- curEcoTest$WTD)^2)/sum((mean(curEcoTest$WTD)-curEcoTest$WTD)^2)
      what$corr[i]<- what$corr[i] + cor(pred20,curEcoTest$WTD)
      what$MAE[i]<-what$MAE[i] + mean(abs(pred20 - curEcoTest$WTD))
      print(j)
    }
  }
  print(i)
  print(what$R2[i])
}
what$R2<-what$R2/10
what$corr<-what$corr/10
what$MAE<-what$MAE/10


write.csv(what, "C:/Users/amini/Desktop/Ardalan_Paper_WTD/Results/V2_cv_results.csv",row.names = F)


R2<-0
corr<-0
MAE<-0
folds<-sample(1:10,nrow(dat),replace = T)
for(j in 1:10){
  curEcoTest<-dat[folds==j & dat$Pekel_Occurence<75,]
  FullTrain<-dat[folds!=j | dat$Pekel_Occurence>=75,]
  
  set.seed(123)
  mod<-xgboost(params=list(eta=HPOdat$eta[b],subsample=HPOdat$ss[b],max_depth=HPOdat$md[b],min_child_weight=HPOdat$mcw[b],gamma=HPOdat$gam[b],
                           lambda=HPOdat$lam[b],alpha=HPOdat$alp[b], colsample_bytree=HPOdat$cs[b]),monotone_constraints= get_monotone(colnames(dat)[explanatories]),
               data=xgb.DMatrix(as.matrix(FullTrain[,explanatories]),label=FullTrain$WTD),nrounds=HPOdat$nt[b],verbose = 0)
  pred20<-predict(mod,xgb.DMatrix(as.matrix(curEcoTest[,explanatories])))
  pred20[pred20<0]<-0
  R2<-R2+ 1-sum((pred20- curEcoTest$WTD)^2)/sum((mean(curEcoTest$WTD)-curEcoTest$WTD)^2)
  corr<- corr + cor(pred20,curEcoTest$WTD)
  MAE<-MAE + mean(abs(pred20 - curEcoTest$WTD))
}
R2/10
corr/10
MAE/10


#################################################################################################################
#######################      run cross validation on each region (V3)    ##############################################3
###############################################################################################################
rm(list = ls())
library(raster)
library(xgboost)
rCur<-raster("C:/Users/amini/Desktop/Ardalan_Paper_WTD/Eco_Regions/EcoRegion_Lev1_Ext.tif") # get raster
dat<-readRDS("C:/Users/amini/Desktop/Ardalan_Paper_WTD/V3dat.Rds") #get final data


explanatories<-c(3:7,10,13:17,19:26)


#choose hyperparameters
get_monotone<-function(cnames){
  OUTPUT<-rep(0,length(cnames))
  OUTPUT[cnames %in% c("sand_0_5cm_Ext","sand_100_200cm_Ext","AI_Terra_EMDNA_Ext")]<-1
  OUTPUT[cnames %in% c("Prec_Evap_Ext","TI_Marthews2019","clay_100_200cm_Ext","clay_0_5cm_Ext","Precipitation_1979_2009_DS_Ext")]<- -1
  OUTPUT
}

HPOdat<-read.csv("C:/Users/amini/Desktop/Ardalan_Paper_WTD/Results/HPOdat.csv")
b<-which.max(HPOdat$r2)

result <- extract(rCur, dat[,1:2])
dat$EcoRegion<-result
rm(result)

what<-as.data.frame(table(dat$EcoRegion))
what<-what[what$Freq>100,]

what$R2<-0
what$corr<-0
what$MAE<-0
for(i in 1:nrow(what)){
  allothers<-dat[dat$EcoRegion!=what$Var1[i] | is.na(dat$EcoRegion) | dat$Pekel_Occurence>=75 | dat$HAND_MERIT>30,]
  curEcoData<-dat[dat$EcoRegion==what$Var1[i] & !is.na(dat$EcoRegion) & dat$Pekel_Occurence<75 & dat$HAND_MERIT<=30,]
  if(nrow(curEcoData)>90){
    folds<-sample(1:10,nrow(curEcoData),replace = T)
    for(j in 1:10){
      curEcoTest<-curEcoData[folds==j,]
      curEcoTrain<-curEcoData[folds!=j,]
      FullTrain<-rbind(allothers,curEcoTrain)
      weights_vec<-rep(1,nrow(FullTrain))
      weights_vec[FullTrain$HAND_MERIT>30]<-0.75
      weights_vec[!is.na(FullTrain$Pekel_Occurence) & FullTrain$Pekel_Occurence>=75]<-FullTrain$Pekel_Occurence[!is.na(FullTrain$Pekel_Occurence) & FullTrain$Pekel_Occurence>=75]/100
      
      set.seed(123)
      mod<-xgboost(params=list(eta=HPOdat$eta[b],subsample=HPOdat$ss[b],max_depth=HPOdat$md[b],min_child_weight=HPOdat$mcw[b],gamma=HPOdat$gam[b],
                               lambda=HPOdat$lam[b],alpha=HPOdat$alp[b], colsample_bytree=HPOdat$cs[b]),monotone_constraints= get_monotone(colnames(dat)[explanatories]),
                   data=xgb.DMatrix(as.matrix(FullTrain[,explanatories]),label=FullTrain$WTD),nrounds=HPOdat$nt[b],verbose = 0,weight = weights_vec)
      pred20<-predict(mod,xgb.DMatrix(as.matrix(curEcoTest[,explanatories])))
      pred20[pred20<0]<-0
      what$R2[i]<-what$R2[i]+ 1-sum((pred20- curEcoTest$WTD)^2)/sum((mean(curEcoTest$WTD)-curEcoTest$WTD)^2)
      what$corr[i]<- what$corr[i] + cor(pred20,curEcoTest$WTD)
      what$MAE[i]<-what$MAE[i] + mean(abs(pred20 - curEcoTest$WTD))
      print(j)
    }
  }
  print(i)
  print(what$R2[i])
}
what$R2<-what$R2/10
what$corr<-what$corr/10
what$MAE<-what$MAE/10


write.csv(what, "C:/Users/amini/Desktop/Ardalan_Paper_WTD/Results/V3_cv_results.csv",row.names = F)


R2<-0
corr<-0
MAE<-0
folds<-sample(1:10,nrow(dat),replace = T)
for(j in 1:1){
  curEcoTest<-dat[folds==j & dat$Pekel_Occurence<75 & dat$HAND_MERIT<=30,]
  FullTrain<-dat[folds!=j | dat$Pekel_Occurence>=75 | dat$HAND_MERIT>30,]
  
  weights_vec<-rep(1,nrow(FullTrain))
  weights_vec[FullTrain$HAND_MERIT>30]<-0.75
  weights_vec[!is.na(FullTrain$Pekel_Occurence) & FullTrain$Pekel_Occurence>=75]<-FullTrain$Pekel_Occurence[!is.na(FullTrain$Pekel_Occurence) & FullTrain$Pekel_Occurence>=75]/100
  
  set.seed(123)
  mod<-xgboost(params=list(eta=HPOdat$eta[b],subsample=HPOdat$ss[b],max_depth=HPOdat$md[b],min_child_weight=HPOdat$mcw[b],gamma=HPOdat$gam[b],
                           lambda=HPOdat$lam[b],alpha=HPOdat$alp[b], colsample_bytree=HPOdat$cs[b]),monotone_constraints= get_monotone(colnames(dat)[explanatories]),
               data=xgb.DMatrix(as.matrix(FullTrain[,explanatories]),label=FullTrain$WTD),nrounds=HPOdat$nt[b],verbose = 0,weight = weights_vec)
  pred20<-predict(mod,xgb.DMatrix(as.matrix(curEcoTest[,explanatories])))
  pred20[pred20<0]<-0
  R2<-R2+ 1-sum((pred20- curEcoTest$WTD)^2)/sum((mean(curEcoTest$WTD)-curEcoTest$WTD)^2)
  corr<- corr + cor(pred20,curEcoTest$WTD)
  MAE<-MAE + mean(abs(pred20 - curEcoTest$WTD))
}
R2/1
corr/1
MAE/1


#################################################################################################################
#######################      run cross validation on each region (Fan predictions)     ##############################################3
###############################################################################################################
rm(list = ls())
library(raster)
library(xgboost)
rCur<-raster("C:/Users/amini/Desktop/Ardalan_Paper_WTD/Eco_Regions/EcoRegion_Lev1_Ext.tif") # get raster
dat<-readRDS("C:/Users/amini/Desktop/Ardalan_Paper_WTD/V1dat.Rds") #get final data
result <- extract(rCur, dat[,1:2])


dat$EcoRegion<-result
rm(result)

dat<-dat[!is.na(dat$Fan_WTD_Sim),]
what<-as.data.frame(table(dat$EcoRegion))
what<-what[what$Freq>100,]


what$R2<-NA
what$corr<-NA
what$MAE<-NA
what$medobs<-NA
for(i in 1:nrow(what)){
  curEcoData<-dat[dat$EcoRegion==what$Var1[i] & !is.na(dat$EcoRegion),]
  pred20<-curEcoData$Fan_WTD_Sim
  what$R2[i]<-1-sum((pred20- curEcoData$WTD)^2)/sum((mean(curEcoData$WTD)-curEcoData$WTD)^2)
  what$corr[i]<-cor(pred20,curEcoData$WTD)
  what$MAE[i]<-mean(abs(pred20 - curEcoData$WTD))
  what$medobs[i]<-median(curEcoData$WTD)
  print(i)
}
what

1-sum((dat$Fan_WTD_Sim- dat$WTD)^2)/sum((mean(dat$WTD)-dat$WTD)^2)
cor(dat$Fan_WTD_Sim,dat$WTD)
mean(abs(dat$Fan_WTD_Sim - dat$WTD))
median(dat$WTD)

#################################################################################################################
#######################      run cross validation on each region (DeGraaf predictions)     ##############################################3
###############################################################################################################
rm(list = ls())
library(raster)
library(xgboost)
rCur<-raster("C:/Users/amini/Desktop/Ardalan_Paper_WTD/Eco_Regions/EcoRegion_Lev1_Ext.tif") # get raster
rDe<-raster("C:/Users/amini/Desktop/Ardalan_Paper_WTD/DeGraafData/DeGraaf_WTD_Ext.tif") # get raster
dat<-readRDS("C:/Users/amini/Desktop/Ardalan_Paper_WTD/V1dat.Rds") #get final data
result <- extract(rCur, dat[,1:2])
Degraaf <- extract(rDe, dat[,1:2])


dat$EcoRegion<-result
dat$Degraaf<-Degraaf
rm(result)

dat<-dat[!is.na(dat$Degraaf),]
what<-as.data.frame(table(dat$EcoRegion))
what<-what[what$Freq>100,]


what$R2<-NA
what$corr<-NA
what$MAE<-NA
what$medobs<-NA
for(i in 1:nrow(what)){
  curEcoData<-dat[dat$EcoRegion==what$Var1[i] & !is.na(dat$EcoRegion),]
  pred20<-curEcoData$Degraaf
  what$R2[i]<-1-sum((pred20- curEcoData$WTD)^2)/sum((mean(curEcoData$WTD)-curEcoData$WTD)^2)
  what$corr[i]<-cor(pred20,curEcoData$WTD)
  what$MAE[i]<-mean(abs(pred20 - curEcoData$WTD))
  what$medobs[i]<-median(curEcoData$WTD)
  print(i)
}
what

1-sum((dat$Degraaf- dat$WTD)^2)/sum((mean(dat$WTD)-dat$WTD)^2)
cor(dat$Degraaf,dat$WTD)
mean(abs(dat$Degraaf - dat$WTD))


#################################################################################################################
#######################      run cross validation on each region (Maxwell predictions)     ##############################################3
###############################################################################################################
rm(list = ls())
library(raster)
library(xgboost)
rCur<-raster("C:/Users/amini/Desktop/Ardalan_Paper_WTD/Eco_Regions/EcoRegion_Lev1_Ext.tif") # get raster
dat<-readRDS("C:/Users/amini/Desktop/Ardalan_Paper_WTD/V1dat.Rds") #get final data
result <- extract(rCur, dat[,1:2])


dat$EcoRegion<-result
rm(result)

dat<-dat[!is.na(dat$MaxwellET_WTD_Ext),]
what<-as.data.frame(table(dat$EcoRegion))
what<-what[what$Freq>100,]


what$R2<-NA
what$corr<-NA
what$MAE<-NA
what$medobs<-NA
for(i in 1:nrow(what)){
  curEcoData<-dat[dat$EcoRegion==what$Var1[i] & !is.na(dat$EcoRegion),]
  pred20<-curEcoData$MaxwellET_WTD_Ext
  what$R2[i]<-1-sum((pred20- curEcoData$WTD)^2)/sum((mean(curEcoData$WTD)-curEcoData$WTD)^2)
  what$corr[i]<-cor(pred20,curEcoData$WTD)
  what$MAE[i]<-mean(abs(pred20 - curEcoData$WTD))
  what$medobs[i]<-median(curEcoData$WTD)
  print(i)
}
what

1-sum((dat$MaxwellET_WTD_Ext- dat$WTD)^2)/sum((mean(dat$WTD)-dat$WTD)^2)
cor(dat$MaxwellET_WTD_Ext,dat$WTD)
mean(abs(dat$MaxwellET_WTD_Ext - dat$WTD))



#average statistics across eco zones
rm(list = ls())
gc()
library(raster)
EcoReg1<-raster("C:/Users/amini/Desktop/Ardalan_Paper_WTD/Eco_Regions/EcoRegion_Lev1_Ext.tif") # get raster
OBS_WTD<-raster("C:/Users/amini/Desktop/Ardalan_Paper_WTD/Rasters/WTD_Unconf_Med_Ext.tif")

for(i in 3:15){
  OBS_DAT<-OBS_WTD[EcoReg1==i & !is.na(OBS_WTD)]
  OBS_DAT<-OBS_DAT[OBS_DAT>0 & OBS_DAT<1000]
  print(paste0("ECOREGION LEVEL 1= ",i, ", Median OBS WTD =",median(OBS_DAT)))
  print(paste0("ECOREGION LEVEL 1= ",i, ", Mean OBS WTD =",mean(OBS_DAT)))
}



# process Ardalan's hydrosheds delineated gaining/losing results
# rm(list = ls())
# HL_dat <- read.csv("C:/Users/amini/Desktop/Ardalan_Paper_WTD/Eco_Regions/HydroLakes_Joined_Pekel_EcoReg.csv")
# HL_dat$Poly_src<-NULL
# HL_dat<-HL_dat[HL_dat$Country %in% c("Canada","United States of America"),]
# HL_dat$Grand_id<-NULL
# HL_dat<-HL_dat[HL_dat$Lake_type==1,]
# HL_dat$Lake_type<-NULL
# HL_dat$Shore_dev<-NULL
# HL_dat$Continent<-NULL
# HL_dat$Vol_res<-NULL
# HL_dat$Vol_src<-NULL
# HL_dat$Wshd_area<-NULL
# HL_dat$EcoRegion_<-NULL
# HL_dat<-HL_dat[!is.na(HL_dat$Pekel_max),]
# HL_dat<-HL_dat[!is.na(HL_dat$HydL_El),]
# HL_dat<-HL_dat[!is.na(HL_dat$EcoRegio_1),]
# HL_dat<-HL_dat[!(HL_dat$EcoRegio_1 %in% c(1,2,4,14)),]
# 
# HL_dat$HydL_El_depth<-HL_dat$HydL_mean- (HL_dat$Elevation- HL_dat$Depth_avg)
# HL_dat$HydL_El<-HL_dat$HydL_mean- HL_dat$Elevation
# 
# 
# HL_dat$Pekel_losing<-as.numeric(HL_dat$Pekel_max<50)
# HL_dat$Pekel_gaining<-as.numeric(HL_dat$Pekel_max>90)
# 
# HL_dat$Pred_losing<-as.numeric(HL_dat$HydL_El< 0 & HL_dat$HydL_El_depth< 0)
# HL_dat$Pred_gaining<-as.numeric(HL_dat$HydL_El> 0)
# HL_dat$Pred_intermed<-as.numeric(HL_dat$HydL_El<0 & HL_dat$HydL_El_depth>0)
# 
# HL_dat<-HL_dat[!(HL_dat$Pekel_losing==0 & HL_dat$Pekel_gaining==0),]
# HL_dat<-HL_dat[!(HL_dat$Pred_losing==0 & HL_dat$Pred_gaining==0),]
# 
# mean(HL_dat$Pekel_gaining== HL_dat$Pred_gaining)
# 
# sum(HL_dat$Pekel_gaining==1 & HL_dat$Pred_gaining==1)/sum(HL_dat$Pekel_gaining==1) # true positive
# sum(HL_dat$Pekel_losing==1 & HL_dat$Pred_losing==1)/sum(HL_dat$Pekel_losing==1) # true negative
# 
# ecoRegs<-sort(unique(HL_dat$EcoRegio_1))
# for(i in 1:length(ecoRegs)){
#   curEco<-ecoRegs[i]
#   curLakes<-HL_dat[HL_dat$EcoRegio_1==curEco,]
#   if(nrow(curLakes)>10 & sum(curLakes$Pekel_losing)>2 & sum(curLakes$Pekel_gaining)>2) print(paste("For Eco Region",curEco,", the overall accuracy is:",
#                                     round(mean(curLakes$Pekel_gaining == curLakes$Pred_gaining),4),
#                                     ", the TPR is:",round(sum(curLakes$Pekel_gaining==1 & curLakes$Pred_gaining==1)/sum(curLakes$Pekel_gaining==1),4),
#                                     ", the TNR is:",round(sum(curLakes$Pekel_losing==1 & curLakes$Pred_losing==1)/sum(curLakes$Pekel_losing==1),4),
#                                     ", # Pekel Gaining:", sum(curLakes$Pekel_gaining),
#                                     ", # Pekel Losing:", sum(curLakes$Pekel_losing)))
# }
# 
# 
# # process Ardalan's pekel delineated gaining/losing results
# rm(list = ls())
# Pekel_GT90_EcoReg_Final <- read.csv("C:/Users/amini/Desktop/Ardalan_Paper_WTD/Eco_Regions/Pekel_GT90_EcoReg_Final.csv")
# Pekel_ST75_EcoReg_Final_Filtered <- read.csv("C:/Users/amini/Desktop/Ardalan_Paper_WTD/Eco_Regions/Pekel_ST75_EcoReg_Final_Filtered.csv")
# Pekel_GT90_EcoReg_Final<-Pekel_GT90_EcoReg_Final[complete.cases(Pekel_GT90_EcoReg_Final),]
# Pekel_ST75_EcoReg_Final_Filtered<-Pekel_ST75_EcoReg_Final_Filtered[complete.cases(Pekel_ST75_EcoReg_Final_Filtered),]
# 
# plot(Pekel_GT90_EcoReg_Final$HydL_El,Pekel_GT90_EcoReg_Final$Pekel_mean)
# cor(Pekel_GT90_EcoReg_Final$HydL_El,Pekel_GT90_EcoReg_Final$Pekel_mean)
# plot(Pekel_ST75_EcoReg_Final_Filtered$HydL_El,Pekel_ST75_EcoReg_Final_Filtered$Pekel_mean)
# cor(Pekel_ST75_EcoReg_Final_Filtered$HydL_El,Pekel_ST75_EcoReg_Final_Filtered$Pekel_mean)
# 
# ecoRegs90<-sort(unique(Pekel_GT90_EcoReg_Final$EcoRegion_))
# ecoRegs75<-sort(unique(Pekel_ST75_EcoReg_Final_Filtered$EcoRegion_))
# ecoRegs<-union(ecoRegs75,ecoRegs90)
# for(i in 1:length(ecoRegs)){
#   curEco<-ecoRegs[i]
#   cur90<-Pekel_GT90_EcoReg_Final[Pekel_GT90_EcoReg_Final$EcoRegion_==curEco,]
#   cur75<-Pekel_ST75_EcoReg_Final_Filtered[Pekel_ST75_EcoReg_Final_Filtered$EcoRegion_==curEco,]
#   cur90$gaining<-as.numeric(cur90$HydL_El>10)
#   cur90$losing<-as.numeric(cur90$HydL_El< -10)
#   cur75$gaining<-as.numeric(cur75$HydL_El> 10)
#   cur75$losing<-as.numeric(cur75$HydL_El< -10)
#   cur90<-cur90[(cur90$gaining==1 | cur90$losing==1),]
#   cur75<-cur75[(cur75$gaining==1 | cur75$losing==1),]
#   
#   if(nrow(cur90)>10 & nrow(cur75)>10) print(paste("For Eco Region",curEco,", the overall accuracy is:",
#                                     round(mean(c(cur90$gaining,cur75$losing)),4),
#                                     ", the TPR is:",round(sum(cur90$gaining)/nrow(cur90),4),
#                                     ", the TNR is:",round(sum(cur75$losing)/nrow(cur75),4),
#                                     ", T is:", nrow(cur90),
#                                     ", P is:", sum(cur90$gaining),
#                                     ", N is:", nrow(cur75)))
#   
# }
# 
# 
# #compare lake gainging/losing with fan and maxwell
# rm(list = ls())
# HL_dat <- read.csv("C:/Users/amini/Desktop/Ardalan_Paper_WTD/Eco_Regions/HydroLakes_AllHydL_Pekel_20230115.csv")
# 
# 
# 
# HL_dat$Poly_src<-NULL
# HL_dat<-HL_dat[HL_dat$Country %in% c("Canada","United States of America"),]
# HL_dat$Grand_id<-NULL
# HL_dat<-HL_dat[HL_dat$Lake_type==1,]
# HL_dat$Lake_type<-NULL
# HL_dat$Shore_dev<-NULL
# HL_dat$Continent<-NULL
# HL_dat$Vol_res<-NULL
# HL_dat$Vol_src<-NULL
# HL_dat$Wshd_area<-NULL
# HL_dat<-HL_dat[!is.na(HL_dat$Pek_max),]
# HL_dat<-HL_dat[!is.na(HL_dat$EcoR_major),]
# #HL_dat<-HL_dat[!(HL_dat$EcoR_major %in% c(1,2,4,14)),]
# HL_dat$Pekel_losing<-as.numeric(HL_dat$Pek_max<80)
# HL_dat$Pekel_gaining<-as.numeric(HL_dat$Pek_max>90)
# HL_dat<-HL_dat[HL_dat$Lake_area>0.5,]
# HL_dat<-HL_dat[HL_dat$Lake_area<40,]
# 
# 
# HL_dat<-HL_dat[!is.na(HL_dat$V14HydL_me),]
# HL_dat$HydL_El_depth<-HL_dat$V14HydL_me- (HL_dat$Elevation- HL_dat$Depth_avg)
# HL_dat$HydL_El<-HL_dat$V14HydL_me- HL_dat$Elevation
# HL_dat$Pred_losing<-as.numeric(HL_dat$HydL_El< 0 & HL_dat$HydL_El_depth< 0)
# HL_dat$Pred_gaining<-as.numeric(HL_dat$HydL_El> 0)
# 
# 
# HL_dat<-HL_dat[!is.na(HL_dat$FanHydL_me),]
# HL_dat$Fan_El_depth<-HL_dat$FanHydL_me- (HL_dat$Elevation- HL_dat$Depth_avg)
# HL_dat$Fan_El<-HL_dat$FanHydL_me- HL_dat$Elevation
# #HL_dat<-HL_dat[(HL_dat$Fan_El< 0 & HL_dat$Fan_El_depth< 0) | HL_dat$Fan_El> 0,]
# HL_dat$Pred_losing<-as.numeric(HL_dat$Fan_El< 0 & HL_dat$Fan_El_depth< 0)
# HL_dat$Pred_gaining<-as.numeric(HL_dat$Fan_El> 0)
# 
# 
# HL_dat<-HL_dat[!is.na(HL_dat$ParFlow_Hy),]
# HL_dat$Parflow_El_depth<-HL_dat$ParFlow_Hy- (HL_dat$Elevation- HL_dat$Depth_avg)
# HL_dat$Parflow_El<-HL_dat$ParFlow_Hy- HL_dat$Elevation
# HL_dat$Pred_losing<-as.numeric(HL_dat$Parflow_El< 0 & HL_dat$Parflow_El_depth< 0)
# HL_dat$Pred_gaining<-as.numeric(HL_dat$Parflow_El> 0)
# 
# 
# HL_dat<-HL_dat[!(HL_dat$Pekel_losing==0 & HL_dat$Pekel_gaining==0),]
# HL_dat<-HL_dat[!(HL_dat$Pred_losing==0 & HL_dat$Pred_gaining==0),]
# 
# mean(HL_dat$Pekel_gaining== HL_dat$Pred_gaining)
# 
# sum(HL_dat$Pekel_gaining==1 & HL_dat$Pred_gaining==1)/sum(HL_dat$Pekel_gaining==1) # true positive
# sum(HL_dat$Pekel_losing==1 & HL_dat$Pred_losing==1)/sum(HL_dat$Pekel_losing==1) # true negative
# 
# sum(HL_dat$Pekel_gaining==1)
# sum(HL_dat$Pred_gaining==1)
# 
# ecoRegs<-sort(unique(HL_dat$EcoR_major))
# for(i in 1:length(ecoRegs)){
#   curEco<-ecoRegs[i]
#   curLakes<-HL_dat[HL_dat$EcoR_major==curEco,]
#   if(nrow(curLakes)>10 & sum(curLakes$Pekel_losing)>2 & sum(curLakes$Pekel_gaining)>2) print(paste("For Eco Region",curEco,", the overall accuracy is:",
#                                                                                                   round(mean(curLakes$Pekel_gaining == curLakes$Pred_gaining),4),
#                                                                                                    ", the TPR is:",round(sum(curLakes$Pekel_gaining==1 & curLakes$Pred_gaining==1)/sum(curLakes$Pekel_gaining==1),4),
#                                                                                                    ", the TNR is:",round(sum(curLakes$Pekel_losing==1 & curLakes$Pred_losing==1)/sum(curLakes$Pekel_losing==1),4),
#                                                                                                    ", # Pekel Gaining:", sum(curLakes$Pekel_gaining),
#                                                                                                    ", # Total:", nrow(curLakes),
#                                                                                                   ", # Pred Gaining:", sum(curLakes$Pred_gaining)))
# }
# 


###############################################################################################################
#######################################   get correlation matrix   ############################################
###############################################################################################################

library(corrplot)
Cornames<-c("Fan et al. (2013)","de Graaf et al. (2015)","Maxwell et al. (2016)","V1","V2","V3","Observations","HAND")

FanCor<-c(1,0.33458,0.000324,0.235213,0.187169,0.520293,0.389775,0.509788)
DegraafCor<-c(0.33458,1,0.000262,0.171695,0.105527,0.607153,0.208467,0.490245)
MaxwellCor<-c(0.000324,0.000262,1,0.381861,0.257444,0.622998,0.087034,0.000128)
V1Cor<-c(0.235213,0.171695,0.381861,1,0.779942,0.162113,0.885223,0.0283)
V2Cor<-c(0.187169,0.105527,0.257444,0.779942,1,0.109909,0.878579,-0.046491)
V3Cor<-c(0.520293,0.607153,0.622998,0.162113,0.109909,1,0.588236,0.779685)
ObsCor<-c(0.389775,0.208467,0.087034,0.885223,0.878579,0.588236,1,0.092322)
HandCor<-c(0.509788,0.490245,0.000128,0.0283,-0.046491,0.779685,0.092322,1)

Cormat<-rbind(FanCor,DegraafCor,MaxwellCor,V1Cor,V2Cor,V3Cor,ObsCor,HandCor)
colnames(Cormat)<-Cornames
rownames(Cormat)<-Cornames
corrplot(Cormat, tl.col="black", tl.srt=45,method="color",addCoef.col = "black",is.corr = F,outline = T)

jpeg(paste0("C:/Users/amini/Desktop/Ardalan_Paper_WTD/Paper/Figures/Corrplot.jpeg"),width = 7, height = 5.8, units = 'in',res = 500)
corrplot(Cormat, tl.col="black", tl.srt=45,method="color",addCoef.col = "black",is.corr = F,outline = T)
dev.off()


###############################################################################################################
#######################################   get feature importance plot   ############################################
###############################################################################################################
library(gplots)

Varnames<-c("Aridity Index","Temperature","January Temperature","Rainfall Intensity","Maximum SWE","Precipitation"
            ,"Snow Fraction","Precipitation Excess","Depth to Bedrock","Shallow Silt Fraction","Deep Silt Fraction"
            ,"Shallow Clay Fraction","Deep Clay Fraction","Shallow Sand Fraction","Deep Sand Fraction","Land Cover"
            ,"Elevation","Slope","Topographic Index")


V1Imp<-c(0.1648899,0.10851259,0.082090957,0.076403595,0.060526124,0.057912623
         ,0.036995587,0.018869906,0.055073481,0.039061719,0.042104366
         ,0.014490126,0.009255401,0.01228033,0.011181188,0.018277432
         ,0.132270016,0.033398412,0.026406219)
V2Imp<-c(0.13678682,0.07299525,0.04602838,0.08243256,0.02592447,0.04748254
         ,0.03751794,0.07858748,0.04719826,0.03298819,0.04584785
         ,0.05317514,0.04562676,0.03531523,0.02658948,0.03170326
         ,0.07366577,0.03285649,0.04727814)
V3Imp<-c(0.02237528,0.02263817,0.03256034,0.01772236,0.01601104,0.01234096
         ,0.01727876,0.01325024,0.03442903,0.01725148,0.01195737
         ,0.01919494,0.01232394,0.01680594,0.01705601,0.02970726
         ,0.16554362,0.1483413,0.37321195)

AllImp<-rbind(V1Imp,V2Imp,V3Imp)



jpeg(paste0("C:/Users/amini/Desktop/Ardalan_Paper_WTD/Paper/Figures/Impplot.jpeg"),width = 5, height = 6, units = 'in',res = 500)
par(mar = c(2, 5.1, 0.25, 0.25)) 
bp <- barplot(AllImp, beside = TRUE, horiz = TRUE, 
              names.arg = Varnames, mgp=c(0,0.25,0),ylim = c(3.5,length(Varnames)*4),
              col = c("lightblue3", "mistyrose", "lightcyan"), xlim = c(0, max(AllImp)), 
              legend = c("V1", "V2", "V3"),args.legend = list(x = "bottomright", cex = .75),las=2,
              cex.names = 0.555,cex.axis = 0.7, tck=-0.008)
dev.off()





