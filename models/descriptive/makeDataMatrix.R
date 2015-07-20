# Make data matrix concatenated from various sources
# 16-Jun-15 Yen Low

RScriptPath="/mnt/hgfs/scripts"
ProjectPath="/mnt/hgfs/projects"
#RScriptPath="/home/yenlow/scripts"
#ProjectPath="/home/yenlow/projects"
setwd(paste(ProjectPath,"/insight/models/descriptive",sep=""))
getwd()

source(paste(RScriptPath,"/R/utils.R",sep=""))
source(paste(RScriptPath,"/R/preprocess.R",sep=""))

require(choroplethrZip)

#load all the data sources
load(file=paste0(ProjectPath,"/insight/data/storeChar.RData"),verbose=T)  #store factors
load(file=paste0(ProjectPath,"/insight/data/cbp.RData"),verbose=T)  #CBP biz climate factors (number of businesses, not normalized by population)
load(file=paste0(ProjectPath,"/insight/data/competition.RData"),verbose=T)  #compet indices
load(file=paste0(ProjectPath,"/insight/data/chr.RData"),verbose=T)  #health indices
#TODO add yelp data

#get zip to county mappings
data(zip.regions)
zip.regions$county.fips=formatC(zip.regions$county.fips.numeric,width=5,format='d',flag='0')
head(sort(zip.regions$county.fips))


lsos()
rm(allStores,bizTypeDf,relTypeDf,cbpTypeAsFeatures,store_county,store_state)
gci()
lsos()

#1602 stores
#map zip to county in order to get county-level health data later
tmp=merge(storeDf,zip.regions[,c("region","county.name","county.fips")], by.x="zip", by.y="region",all.x=T)
#don't double count same store in 2 counties (cos 1 zip can map to 2 counties)
#delete the second county
tmp=tmp[order(tmp$zip,tmp$county.fips),]
dsStore=tmp[!duplicated(tmp$zip),] #1437 stores
dim(dsStore)

#dsStore=merge(dsStore,df_pop_zip,by.x="zip",by.y="region",all.x=T)  #population is also provided in CHR
dsStore=merge(dsStore,competDf,by.x="zip",by.y=0,all.x=T)     #add compet data
dsStore=merge(dsStore,chrDf,by.x="county.fips",by.y="fip",all.x=T)    #add health data

#normalize biz data (number of businesses) to rates (number of businesses per 100,000 ppl)
cbpRates=merge(dsStore[,c("zip","X2011.population.estimate")],cbpDf,by.x="zip",by.y="zip",all.x=T)
colnames(cbpRates)
excludeID=grep("^zip$|population|^city$|AnnPayrollK",colnames(cbpRates))

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


#rename columns
colnames(dsStore)=gsub("^retail.x$","retail",colnames(dsStore))
colnames(dsStore)=gsub("^retail.y$","retailInd",colnames(dsStore))
colnames(dsStore)=gsub("^unclassified$","nUnclassifiedBiz",colnames(dsStore))
colnames(dsStore)=gsub("^overall$","nClassifiedBiz",colnames(dsStore))
colnames(dsStore)=gsub("^X2011.population.estimate","population",colnames(dsStore))
colnames(dsStore)=gsub("^county.name$","county",colnames(dsStore))
colnames(dsStore)=gsub("^county.fips$","fip",colnames(dsStore))
colnames(dsStore)

######ensure variables are of right type
#find which variables are of which type
factorColID=which(colType(dsStore,"is.factor"))
characterColID=which(colType(dsStore,"is.character"))
integerColID=which(colType(dsStore,"is.integer"))
doubleColID=which(colType(dsStore,"is.double"))

#identify variables to convert
var2char="city"
var2factor=c("busType", "bizType")
var2integer=c("population", "numCompetitor", "numClient")
var2double=c("Median.household.income", "Premature.death", "Sexually.transmitted.infections", "Other.primary.care.providers",
             "Mental.health.providers", "Violent.crime", "HIV.prevalence.rate", "Health.care.costs")

#convert variable types
dsStore[,var2char]=convertType(dsStore,var2char,"as.character")
dsStore[,var2factor]=convertType(dsStore,var2factor,"as.factor")
#need to convert factor to character then numeric
dsStore[,var2integer]=convertType(dsStore,var2integer,"toNumWithoutComma")
#need to convert factor to character then numeric
dsStore[,var2double]=convertType(dsStore,var2double,"toNumWithoutComma")
dim(dsStore)   #1437  117

dsStore$storesPerCapita=(dsStore$numClient + dsStore$numCompetitor)/dsStore$population*100000

#drop DEA_NUM,"numClient","busType","bizType"
dsStore=dsStore[,-grep("DEA_NUM|numClient|CODE$|^County$|^State$|not.ranked$|nEmployed",colnames(dsStore))]   #drop columns
dim(dsStore)    #1437  110
colnames(dsStore)


#reorder cols by type
locVar=colnames(dsStore)[grep("^county$|zip|fip|^city$|^state$",colnames(dsStore),ignore.case=T)]
popVar=colnames(dsStore)[grep("population",colnames(dsStore),ignore.case=T)]
insureVar=colnames(dsStore)[grep("insure",colnames(dsStore),ignore.case=T)]
employVar=colnames(dsStore)[grep("employ",colnames(dsStore),ignore.case=T)]
incomeVar=colnames(dsStore)[grep("income",colnames(dsStore),ignore.case=T)]
subsetOrder=c("storeID","zip","city","fip","county","state"
              ,"busType","bizType","retail","medical","others",
              popVar,insureVar,employVar,incomeVar,"storesPerCapita")
otherVar=setdiff(colnames(dsStore),subsetOrder)
dsStore=dsStore[,c(subsetOrder,otherVar)] 
colnames(dsStore)
dim(dsStore)   #1437  110


save(dsStore,file=paste0(ProjectPath,"/insight/data/dsStore.RData"))
#load(file=paste0(ProjectPath,"/insight/data/dsStore.RData"),verbose=T)


#for writing to a quick table for viewing
colnames(dsStore)
wantedCol=c("storeID","zip","city","fip","county","state","bizType","population","Uninsured","Unemployment","storesPerCapita","competDensity","nBiz")
dsMap=dsStore[,wantedCol]
write.table(dsMap,file=paste0(ProjectPath,"/insight/data/dsMap.tsv"),sep="\t",quote=F,col.names=F)
