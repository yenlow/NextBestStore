#set paths
RScriptPath="/mnt/hgfs/scripts"
ProjectPath="/mnt/hgfs/projects"
setwd(paste(ProjectPath,"/insight/models/maps",sep=""))
getwd()

source(paste(RScriptPath,"/R/utils.R",sep=""))
source(paste(RScriptPath,"/R/preprocess.R",sep=""))
source(paste(RScriptPath,"/R/localMySQL.R",sep=""))

require(RMySQL)
require(reshape2)
require(ggplot2)
require(data.table)
require(dplyr)
#suppressPackageStartupMessages(require(googleVis))
#op=options(gvis.plot.tag=NULL)

#get zip to long, lat
require(zipcode)
data(zipcode)

#Load data
load(file=paste0(ProjectPath,"/insight/data/dsStore.RData"),verbose=T)  #store level
load(file=paste0(ProjectPath,"/insight/models/fa/FA.RData"),verbose=T)  #factor scores
load(file=paste0(ProjectPath,"/insight/models/glmmlasso_fa/datamat.RData"),verbose=T)  #feature matrix
load(file=paste0(ProjectPath,"/insight/data/storeChar.RData"),verbose=T)     #stores grouped by zip, county, state
load(file=paste0(ProjectPath,"/insight/data/competition.RData"),verbose=T)
load(file=paste0(ProjectPath,"/insight/data/revGrowth.RData"),verbose=T)  #response: sales growthload
load(file=paste0(ProjectPath,"/insight/models/predict/datamat_pred.RData"),verbose=T)  #features of nonstores
load(file=paste0(ProjectPath,"/insight/models/predict/pred.RData"),verbose=T)  #predicted values of nonstores
load(file=paste0(ProjectPath,"/insight/models/predict/ts.RData"),verbose=T)  #predicted values of nonstores
rm(storeDf,allStores,store_county,store_state,store_zip,tsList,tsList_pred)
lsos()
gci()

#get item volume change and rev
con = dbConnect(drv, username=username, password=passwd,"testdb")
drugDf=dbGetQuery(con,"SELECT * FROM itemChange order by Customer_Identifier, familyID, yr_month")
dbDisconnect(con)

#calculate change in vol and itemRevenue between end (201505) and start (201312)
drugDf=as.data.table(drugDf)
ind=c("Customer_Identifier","familyID")
setkeyv(drugDf,ind)
drugDf[,c('volChange','revChange'):=list(vol-vol[1], itemRevenue-itemRevenue[1]),by=ind]
drugDf[1:10,]
changeDf=subset(drugDf,yr_month=="201505")
changeDf=merge(as.data.frame(changeDf), dsStore[,c("storeID","zip")],by.x="Customer_Identifier",by.y="storeID")
changeDf=changeDf[order(changeDf$zip,changeDf$Customer_Identifier,-changeDf$volChange),]


#### plot zip locations of all stores (clients and competitors)
#remove 134 stores with short time series (> 12 mths)
numMonths=aggregate(timeOrder~storeID,data=datamat[,c("timeOrder","storeID")],function(x) length(unique(x)))
table(numMonths$timeOrder)
rmStores=numMonths[numMonths$timeOrder<13,"storeID"]
length(rmStores)   #134 stores

bestStores=setdiff(rownames(aveGrowthDf)[order(aveGrowthDf$aveQtrGrowth,decreasing=TRUE)][1:100],rmStores)
worstStores=setdiff(rownames(aveGrowthDf)[order(aveGrowthDf$aveQtrGrowth,decreasing=F)][1:100],rmStores)

zip_bestStores=unique(dsStore[dsStore$storeID %in% bestStores,"zip"])
zip_worstStores=unique(dsStore[dsStore$storeID %in% worstStores,"zip"])


#rbind zip features of stores and nonstores
#store data all have storeID but not zip (unless mapped from dsStore)
#nonstore (pred) data all have zip but not storeID (storeID left as NA)
#merge them in a way that they both have zip and storeID
sum(is.na(dsStore$zip))   #check that dsStore has zip values
dsStore_pred$zip=rownames(dsStore_pred)
dsStore_all=bind_rows(dsStore,dsStore_pred)
sum(is.na(dsStore_all$zip))
sum(duplicated(dsStore_all$zip))   #should not be duplicated

colnames(dsFA_pred)=colnames(factorScores)
factorScores=as.data.frame(factorScores)
factorScores$storeID=rownames(factorScores)
factorScores$zip=dsStore$zip[match(factorScores$storeID,dsStore$storeID)]
dsFA_pred=as.data.frame(dsFA_pred)
dsFA_pred$zip=rownames(dsFA_pred)
dsFA_all=bind_rows(factorScores,dsFA_pred)
sum(is.na(dsFA_all$zip))
sum(duplicated(dsFA_all$zip))    #should be not duplicated

aveGrowthDf$storeID=rownames(aveGrowthDf)
aveGrowthDf$zip=dsStore$zip[match(aveGrowthDf$storeID,dsStore$storeID)]
aveGrowthDf_pred$zip=rownames(aveGrowthDf_pred)
aveGrowthDf_all=bind_rows(aveGrowthDf,aveGrowthDf_pred)
tmp=aveGrowthDf_all[is.na(aveGrowthDf_all$zip),]
sum(is.na(aveGrowthDf_all$zip))     #153 stores were removed due to lack of zip features
aveGrowthDf_all=aveGrowthDf_all[!is.na(aveGrowthDf_all$zip),]
sum(is.na(aveGrowthDf_all$zip))
sum(duplicated(aveGrowthDf_all$zip)) #should be not duplicated

#get time series
timeseriescols=dcast(datamat, timeOrder ~ storeID, value.var="totalRevenue")
timeseriescols_pred=dcast(predMat, timeOrder ~ zip, value.var="totRev_pred")

#sum revenue
#estimate from mean x 12 months (to negate NA effects if revenue for certain mths are missing)
annRev=colMeans(timeseriescols[,-1],na.rm=T)*12
annRev_pred=colMeans(timeseriescols_pred[,-1],na.rm=T)*12

#convert time series into string separated by commas (required by sparklines)
tsStrings=apply(round(timeseriescols[,-1]),2,paste,collapse=",")
tsStrings_pred=apply(round(timeseriescols_pred[,-1]),2,paste,collapse=",")

tsStrings=data.frame(revenue=tsStrings,annRev=annRev)
tsStrings$revenue=as.character(tsStrings$revenue)
tsStrings$storeID=rownames(tsStrings)
tsStrings$zip=dsStore$zip[match(tsStrings$storeID,dsStore$storeID)]
tsStrings_pred=data.frame(revenue=tsStrings_pred,annRev=annRev_pred)
tsStrings_pred$revenue=as.character(tsStrings_pred$revenue)
tsStrings_pred$zip=rownames(tsStrings_pred)
tsStrings_all=bind_rows(tsStrings,tsStrings_pred)
sum(is.na(tsStrings_all$zip))
sum(duplicated(tsStrings_all$zip)) #should be not duplicated


#create matrix for map
# tmp=merge(competDf[zip_bestStores,],zipcode[,c("zip","city","state","latitude","longitude")],by.x=0  ,by.y="zip")
# tmp$best=1
# tmp2=merge(competDf[zip_worstStores,],zipcode[,c("zip","city","state","latitude","longitude")],by.x=0  ,by.y="zip")
# tmp2$best=0
# dsMap=rbind(tmp,tmp2)
dsMap=merge(competDf,zipcode[,c("zip","city","state","latitude","longitude")],by.x=0  ,by.y="zip")
dsMap$loc=paste(round(dsMap$latitude,1),round(dsMap$longitude,1),sep=":")
dsMap$name=paste0(dsMap$city, ", " ,dsMap$state," ", as.character(dsMap$Row.names))

#progressively merged by zip
colnames(dsMap)[grep("Row.names",colnames(dsMap))]="zip"
dsMap=merge(dsMap,dsStore_all,by="zip",sort=F,all.x=T)
dsMap=merge(dsMap,dsFA_all,by="zip",sort=F,all.x=T)
dsMap=merge(dsMap,aveGrowthDf_all,by="zip",sort=F,all.x=T)
dsMap=merge(dsMap,tsStrings_all,by="zip",all.x=T,sort=F)
colnames(dsMap)
dsMap[,grep("storeID",colnames(dsMap))]   #check that storeID are identical across rows
dsMap$revenue=as.character(dsMap$revenue)
dim(dsMap)  #25769   140

#drop repeated colnames
dsMap=dsMap[,-grep("storeID.y|latitude|longitude|city|state.y|county|busType|fip|county|compet.+.y$",colnames(dsMap),ignore.case=T)]
dsMap=dsMap[,-grep("storeID.x.1",colnames(dsMap),ignore.case=T)]
colnames(dsMap)[grep("storeID.y|latitude|longitude|city|state.y|county|busType|fip|county|compet.+.y$",colnames(dsMap),ignore.case=T)]
#rename colnames
colnames(dsMap)[grep("storeID.x",colnames(dsMap))]="storeID"
colnames(dsMap)[grep("numCompetitor.x",colnames(dsMap))]="otherStores"
colnames(dsMap)[grep("numClient",colnames(dsMap))]="ClientsInZip"
colnames(dsMap)[grep("competDensity.x",colnames(dsMap))]="CompetitionDensity"
colnames(dsMap)[grep("state.x",colnames(dsMap))]="State"
colnames(dsMap)[grep("bizType",colnames(dsMap))]="StoreType"
colnames(dsMap)[grep("Unemployment.x",colnames(dsMap))]="UnemploymentRate"
colnames(dsMap)[grep("Uninsured.x",colnames(dsMap))]="UninsuredRate"
#Rename factor scores
colnames(dsMap)[grep("Unemployment.y",colnames(dsMap))]="Unemployment"
colnames(dsMap)[grep("Uninsured.y",colnames(dsMap))]="Uninsured"
colnames(dsMap)

#convert to percentages
dsMap$UnemploymentRate=100*dsMap$UnemploymentRate
dsMap$UninsuredRate=100*dsMap$UninsuredRate
dsMap$storesPerCapita=(dsMap$otherStores + dsMap$ClientsInZip)/dsMap$population*100000
dsMap$annRev=round(dsMap$annRev)

#convert types
doubleColID=which(colType(dsMap,"is.double"))
integerID=grep("^population$|AnnPayrollK|revenue|annRev",colnames(dsMap))
#check remaining columns are not numeric
dsMap[1:3,-c(doubleColID,integerID)]
sigID=setdiff(doubleColID,integerID)
dsMap[,sigID]=signif(dsMap[,sigID],3)
dsMap[1:3,]

#explore zips (SF:941xx, Bay:940xx, NYC:1000x, HI:56071, SD:57756, AK:995)
SFzipID=grep("^577",tsStrings$zip)
SFzipID_pred=grep("^577",tsStrings_pred$zip)
rbind(tsStrings[SFzipID,c("zip","revenue")],tsStrings_pred[SFzipID_pred,c("zip","revenue")])
tmp=dsMap[dsMap$zip %in% c(tsStrings$zip[SFzipID],tsStrings_pred$zip[SFzipID_pred]),]
View(tmp)
tmp[,c("revenue","annRev")]

colnames(dsMap)

dsMap=dsMap[order(dsMap$annRev,decreasing=T),]

#convert to data.table for speed and easy manipulation
DTmap=data.table(dsMap)
DTmap=DTmap[!(State %in% c("AK","HI"))]
setkeyv(DTmap,c("annRev","aveQtrGrowth"))
key(DTmap)
#subset(DTmap[order(-annRev)],is.na(storeID))

save(zip_bestStores, zip_worstStores, bestStores, worstStores,
     dsMap, timeseriescols, timeseriescols_pred, file="mapData.RData")
save(DTmap, file="DTmap.RData")
save(changeDf, file="drugsData.RData")
