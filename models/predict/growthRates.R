# Calculate growth rates from predMat prediction results of hlm_fa
#
# 26-Jun-15 Yen Low

RScriptPath="/mnt/hgfs/scripts"
ProjectPath="/mnt/hgfs/projects"
setwd(paste(ProjectPath,"/insight/models/predict",sep=""))
getwd()

source(paste(RScriptPath,"/R/utils.R",sep=""))
source(paste(RScriptPath,"/R/hampel.R",sep=""))
source(paste(RScriptPath,"/R/growth.R",sep=""))

require(astsa)
require(TTR)
#require(plyr)
```

### Load data
load(file=paste0(ProjectPath,"/insight/models/predict/pred.RData"),verbose=T)  #predicted values of nonstores
predMat

### Data Manipulation
#Convert to dataframe and time series object
tsDf=xtabs(totRev_pred ~ zip + timeOrder, predMat)
yr_mth=seq(as.Date("2013/12/1"), by = "month", length.out = 18)
colnames(tsDf)=format(yr_mth,"%Y-%m")
head(tsDf,3)

#Then put into a time series object (as ts object from astsa package)
tsfunc<-function(x){
  ans=ts(x,start=1,freq=1)
  return(list(ans))
}
tsList=apply(tsDf,1,tsfunc)
#x=tsList[[1]]


### Calculate growth rates
#Growth rate = (x(tn) - x(tn-1))x(tn-1)
#Average growth rates are calculated from trimmed means of at least 3 values
#Outliers are trimmed by Hampel's test (>3 SD from median)
#Monthly growth
growth_mth=lapply(tsList, function(x) growthrate(x[[1]], n=1))
#x=growth_mth[[1]]
aveMthGrowth=sapply(growth_mth,trimmedMean_hampel,RemoveNAs=T,na.rm=T,minlen=3)

#Quarterly growth
growth_qtr=lapply(tsList, function(x){
  xtsObj=as.xts(x[[1]])
  index(xtsObj)=yr_mth
  newts=to.monthly(xtsObj,indexAt="yearmon")[,4]
  ans=growthrate(newts[-nrow(newts),], n=1)
  return(ans)
})
aveQtrGrowth=sapply(growth_qtr,trimmedMean_hampel,RemoveNAs=T,na.rm=T,minlen=3)
# x=growth_qtr[[1]]
# temp=list()
# for(i in 1:length(growth_qtr)){
#   x=growth_qtr[[i]]
#   temp[[i]]=trimmedMean_hampel(x)
# }

#Annual growth
growth_yr=lapply(tsList, function(x){
  xtsObj=as.xts(x[[1]])
  index(xtsObj)=yr_mth
  newts=to.yearly(xtsObj,indexAt="yearmon")[,4]
  ans=growthrate(newts[-nrow(newts),], n=1)
  return(ans)
})
aveYrGrowth=sapply(growth_yr,trimmedMean_hampel,RemoveNAs=T,na.rm=T,minlen=1)

#View histograms of growth rates
#### Monthly growth
growth_mth[[1]]
#sort(aveMthGrowth)
lim=0.5
tmp=summary(aveMthGrowth)
tmp
hist(aveMthGrowth[abs(aveMthGrowth)<lim],breaks=seq(-lim,lim,by=0.05))
abline(v=tmp["Median"],lty=2)
abline(v=tmp["Mean"])

#### Quarterly growth
growth_qtr[[1]]
#sort(aveQtrGrowth)
lim=1
tmp=summary(aveQtrGrowth[abs(aveQtrGrowth)<lim],)
tmp
hist(aveQtrGrowth[abs(aveQtrGrowth)<lim],breaks=seq(-lim,lim,by=0.1))
abline(v=tmp["Median"],lty=2)
abline(v=tmp["Mean"])

#### Quarterly growth
growth_yr[[1]]
#sort(aveYrGrowth)
tmp=summary(aveYrGrowth[aveYrGrowth>-2 & aveYrGrowth<4],)
tmp
hist(aveYrGrowth[aveYrGrowth>-2 & aveYrGrowth<4],breaks=seq(-2,4,by=0.1))
abline(v=tmp["Median"],lty=2)
abline(v=tmp["Mean"])
legend("topright", c("Mean","Median"),lty=c(1,2))

### Save growth rates and average growth rates into data frames
#Dataframe of average growth rate
aveGrowthDf_pred=as.data.frame(cbind(aveMthGrowth,aveQtrGrowth,aveYrGrowth))
head(aveGrowthDf_pred,3)

#Indicator dataframe of high average growth rate using the above thresholds
#Min 5% average monthly growth, 10% quarterly growth, 25% yearly growth
highGrowthDf_pred=as.data.frame(matrix(0,nrow=nrow(aveGrowthDf_pred),ncol=ncol(aveGrowthDf_pred)))
rownames(highGrowthDf_pred)=rownames(aveGrowthDf_pred)
colnames(highGrowthDf_pred)=c("highMthGrowth","highQtrGrowth","highYrGrowth")
highGrowthDf_pred[which(aveGrowthDf_pred$aveMthGrowth>0.05),"highMthGrowth"]=1
highGrowthDf_pred[which(aveGrowthDf_pred$aveQtrGrowth>0.1),"highQtrGrowth"]=1
highGrowthDf_pred[which(aveGrowthDf_pred$aveYrGrowth>0.25),"highYrGrowth"]=1
highGrowthDf_pred[!is.finite(as.matrix(aveGrowthDf_pred))]=NA
head(highGrowthDf_pred,3)


#Check for NA values
sum(is.na(highGrowthDf_pred))

#save
tsList_pred=tsList
save(tsList_pred,highGrowthDf_pred,aveGrowthDf_pred,file=paste0(ProjectPath,"/insight/models/predict/ts.RData"))
