# Load datamat_pred data matrix of latent factors for prediction by hlm_fa
#
# 20-Jun-15 Yen Low

RScriptPath="/mnt/hgfs/scripts"
ProjectPath="/mnt/hgfs/projects"
#RScriptPath="/home/yenlow/scripts"
#ProjectPath="/home/yenlow/projects"
setwd(paste(ProjectPath,"/insight/models/predict",sep=""))
getwd()

require(nlme)

source(paste(RScriptPath,"/R/utils.R",sep=""))

#load datamat_pred (latent factor matrix)
load(file="datamat_pred.RData",verbose=T)
#datamat_pred=datamat_pred[order(datamat_pred$storeID,datamat_pred$timeOrder),]

#load training data for comparison
load(file="../glmmlasso_fa/datamat.RData",verbose=T)

datamat_pred[1:20,]
tstMat[1:20,]

summary(datamat_pred)
summary(tstMat)

#load model
load(file=paste0(ProjectPath,"/insight/models/hlm_fa/hlm_fa.RData"),verbose=T)   #HLM model of latent factors
attributes(mod_cortime)

#formulate string require for model
varString=paste(setdiff(colnames(datamat_pred),c("storeID","timeOrder")),collapse=" + ")
formulaString4=paste0("totalRevenue ~ timeOrder + I(timeOrder^2) + I(timeOrder^3) + ",varString," + timeOrder*Uninsured + Uninsured*HealthCost + Uninsured*Natal")
formulaString4

#predict
mod_cortime$call
totRev_pred=predict(mod_cortime,newdata=datamat_pred,level=0)
sum(is.na(totRev_pred))

predMat=cbind(datamat_pred[,c("storeID","timeOrder")],totRev_pred)
colnames(predMat)[grep("storeID",colnames(predMat))]="zip"

sumPredRev=aggregate(totRev_pred~zip,predMat,sum,na.rm=T)
sumPredRev=sumPredRev[order(sumPredRev$totRev_pred,decreasing=T),]
sumPredRev$cumsum=cumsum(sumPredRev$totRev_pred)


ticks=10^(1:9)
tickLabels=formatC(ticks, digits=0, format="d",big.mark=",")
png(file="cumRev.png",width=4,height=4,units="in",res=150,bg="transparent")
par(mar=c(5,6,2,1))
plot(sumPredRev$cumsum,log="y",xlab="number of new stores",
     ylab="",
     main="Potential revenue gain (cumulative)",
     type="lines",las=1,cex.axis=0.8,yaxt="n",
     cex.lab=0.7,ylim=c(1e5,1e10))
axis(2,at=c(ticks,1e10),labels=c(tickLabels,"10,000,000,000"),las=2,cex.axis=0.8)
dev.off()

sumPredRev$cumsum[100]

sumPredRev$ID=as.double(1:nrow(sumPredRev))
sumPredRev$cumsum=round(sumPredRev$cumsum)

save(predMat,file="pred.RData")
save(sumPredRev,file="../maps/sumpred.RData")
