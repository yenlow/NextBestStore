# Make dsStore_pred data matrix concatenated from various sources for prediction (has no store-specific info)
# From the 70 variables, convert into factors based on mod_fa
#
# 30-Jun-15 be careful when rescaling. Constant variables are erroneously rescaled to NaN (becos SD=0) 
# 20-Jun-15 Yen Low

RScriptPath="/mnt/hgfs/scripts"
ProjectPath="/mnt/hgfs/projects"
#RScriptPath="/home/yenlow/scripts"
#ProjectPath="/home/yenlow/projects"
setwd(paste(ProjectPath,"/insight/models/predict",sep=""))
getwd()

source(paste(RScriptPath,"/R/utils.R",sep=""))
source(paste(RScriptPath,"/R/preprocess.R",sep=""))

require(choroplethrZip)   #for zip to county mappings

#load all the data sources
load(file=paste0(ProjectPath,"/insight/data/cbp.RData"),verbose=T)  #CBP biz climate factors (number of businesses, not normalized by population)
load(file=paste0(ProjectPath,"/insight/data/competition.RData"),verbose=T)  #compet indices
load(file=paste0(ProjectPath,"/insight/data/chr.RData"),verbose=T)  #health indices
load(file="../glmmlasso/datamat.RData",verbose=T)                #for colmeans and colSDs for rescaling according to training data
load(file=paste0(ProjectPath,"/insight/models/fa/FA.RData"),verbose=T)   #FA model for transforming raw variables into factors

#get zip to county mappings
data(zip.regions)
zip.regions$county.fips=formatC(zip.regions$county.fips.numeric,width=5,format='d',flag='0')
head(sort(zip.regions$county.fips))


lsos()
rm(allStores, bizTypeDf,relTypeDf,cbpTypeAsFeatures)
gci()
lsos()

#1602 stores
#map zip to county in order to get county-level health data later
tmp=merge(competDf[competDf$numClient==0,],zip.regions[,c("region","county.name","county.fips")], by.x=0, by.y="region",all.x=T)
#don't double count same store in 2 counties (cos 1 zip can map to 2 counties)
#delete the second county
colnames(tmp)[grep("Row.names",colnames(tmp))]="zip"
tmp=tmp[order(tmp$zip,tmp$county.fips),]
dsStore=tmp[!duplicated(tmp$zip),] #24333 unique zips
dim(dsStore)   #24333     6

#dsStore=merge(dsStore,df_pop_zip,by.x="zip",by.y="region",all.x=T)  #population is also provided in CHR
dsStore=merge(dsStore,chrDf,by.x="county.fips",by.y="fip",all.x=T)    #add health data

#normalize biz data (number of businesses) to rates (number of businesses per 100,000 ppl)
cbpRates=merge(dsStore[,c("zip","X2011.population.estimate")],cbpDf,by.x="zip",by.y="zip",all.x=T)
colnames(cbpRates)
excludeID=grep("^zip$|population|^city$|AnnPayrollK",colnames(cbpRates))
colnames(cbpRates)[excludeID]

#pop=as.numeric(gsub(",","",as.character(cbpRates$X2011.population.estimate)))
pop=toNumWithoutComma(cbpRates$X2011.population.estimate)
tmp=cbpRates[,-excludeID]
for(i in 1:ncol(tmp)) tmp[,i]=tmp[,i]/pop*100000

cbpRates=cbind(cbpRates[,excludeID],tmp)
dim(cbpRates)
#drop population
cbpRates=cbpRates[,-grep("population",colnames(cbpRates))]
dim(cbpRates)                                 

dsStore=merge(dsStore,cbpRates,by.x="zip",by.y="zip",all.x=T)    #add biz data
dim(dsStore)
colnames(dsStore)

#rm rows with a lot of missing data
dsStore=rmRow(dsStore,maxNA=0.5)
#Drop rows with > maxNA ratio: 5161 
#Drop duplicated rows: 0 

#drop DEA_NUM,"numClient","busType","bizType"
dsStore=dsStore[,-grep("DEA_NUM|numClient|CODE$|^County$|^State$|not.ranked$|nEmployed",colnames(dsStore))]   #drop columns
dim(dsStore)
colnames(dsStore)

#rename columns
colnames(dsStore)=gsub("^retail.x$","retail",colnames(dsStore))
colnames(dsStore)=gsub("^retail.y$","retailInd",colnames(dsStore))
colnames(dsStore)=gsub("^unclassified$","nUnclassifiedBiz",colnames(dsStore))
colnames(dsStore)=gsub("^overall$","nClassifiedBiz",colnames(dsStore))
colnames(dsStore)=gsub("^X2011.population.estimate","population",colnames(dsStore))
colnames(dsStore)=gsub("^county.name$","county",colnames(dsStore))
colnames(dsStore)=gsub("^county.fips$","fip",colnames(dsStore))
colnames(dsStore)

#consider only retail pharmacies (medicalRx tend to be neg)
dsStore$retail=1
dsStore$medical=0
dsStore$others=0

#save(dsStore,file="datamat_pred.RData")

######ensure variables are of right type
#find which variables are of which type
factorColID=which(colType(dsStore,"is.factor"))
characterColID=which(colType(dsStore,"is.character"))
integerColID=which(colType(dsStore,"is.integer"))
doubleColID=which(colType(dsStore,"is.double"))

#identify variables to convert
var2char="city"
#var2factor=c("busType", "bizType")
var2integer=c("population", "numCompetitor")
var2double=c("Median.household.income", "Premature.death", "Sexually.transmitted.infections", "Other.primary.care.providers",
             "Mental.health.providers", "Violent.crime", "HIV.prevalence.rate", "Health.care.costs","Primary.care.physicians","Dentists","AnnPayrollK")

#convert variable types
dsStore[,var2char]=convertType(dsStore,var2char,"as.character")
#dsStore[,var2factor]=convertType(dsStore,var2factor,"as.factor")
#need to convert factor to character then numeric
dsStore[,var2integer]=convertType(dsStore,var2integer,"toNumWithoutComma")
#need to convert factor to character then numeric
dsStore[,var2double]=convertType(dsStore,var2double,"toNumWithoutComma")
dim(dsStore)   #19174   104

#save(dsStore,file="datamat_pred.RData")

#load raw features to transform into factors
#get colnames of raw features required to be transformed
load(file=paste0(ProjectPath,"/insight/data/dsMat.RData"),verbose=TRUE)
rawVar=colnames(dsMat_rmColRow)
setdiff(rawVar,colnames(dsStore))   #check that all rawVar are within colnames of dsStore

dsStore_pred=dsStore[,rawVar]
rownames(dsStore_pred)=dsStore$zip
dim(dsStore_pred) #19174    70

#fill up remaining NA by median imputation (if not more than 50% NA columns)
colMedians=apply(dsStore_pred,2,median,na.rm=T)
for(i in 1:ncol(dsStore_pred)) dsStore_pred[is.na(dsStore_pred[,i]),i]=colMedians[i]

#convert into factor scores
attributes(mod_fa)
wts <- mod_fa$loadings
scaled <- scale(dsMat_rmColRow) 
#tmp <-t( apply(scaled,1,function(x) colSums(x*wts,na.rm=TRUE))) 
tmp=scaled %*%  mod_fa$loading   #equiv to above
#tmp=factor.scores(dsMat_rmColRow,mod_fa$loadings,method="components")   #equiv to above
summary(tmp[,1:5])
summary(mod_fa$scores[,1:5])

columnMean=attr(scaled,"scaled:center")
columnSD=attr(scaled,"scaled:scale")

#rescale to training matrix scales
dsStore_predScaled=matrix(NA,nrow=nrow(dsStore_pred),ncol=ncol(dsStore_pred))
dimnames(dsStore_predScaled)=dimnames(dsStore_pred)
for(i in colnames(dsStore_pred))  dsStore_predScaled[,i]=(dsStore_pred[,i]-columnMean[i])/columnSD[i]
#save(dsStore_pred,file="datamat_pred.RData")

summary(dsStore_pred)
dsFA_pred=dsStore_predScaled %*%  mod_fa$loadings#dot product
#DON'T USE factor.scores AS IT WILL RESCALE DATA (constant variables will be inflated into NaN)
#dsFA_pred=factor.scores(dsStore_pred,mod_fa,method="components")
summary(dsFA_pred[,1:5])
summary(mod_fa$scores[,1:5])
dim(dsFA_pred)  #19174    15
#check for NA values
sum(is.na(dsFA_pred))

#create tall fake table to form cross-cartesian with dsFA_pred
tmp=data.frame(zip=rep(rownames(dsStore_pred),each=18),timeOrder=rep(1:18,nrow(dsStore_pred)))
nrow(tmp)/18   #19174 zips
tmp[1:20,]

datamat_pred=merge(dsFA_pred,tmp,by.x=0,by.y="zip")
dim(datamat_pred)
nrow(datamat_pred)/18   #19174 zips
colnames(datamat_pred)[grep("Row.names",colnames(datamat_pred))]="storeID"
colnames(datamat_pred)[sort(grep("^MR",colnames(datamat_pred)))]=colnames(factorScores)

datamat_pred=datamat_pred[order(datamat_pred$storeID,datamat_pred$timeOrder),]
save(dsFA_pred,dsStore_pred,datamat_pred,file="datamat_pred.RData")


