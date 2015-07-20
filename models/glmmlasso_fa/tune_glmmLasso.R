# Run glmmLasso of all variables on AWS
# 18-Jun-15 Yen Low


#RScriptPath="/mnt/hgfs/scripts"
#ProjectPath="/mnt/hgfs/projects"
RScriptPath="/home/yenlow/scripts"
ProjectPath="/home/yenlow/projects"
setwd(paste(ProjectPath,"/insight/models/glmmlasso",sep=""))
getwd()

source(paste(RScriptPath,"/R/utils.R",sep=""))

require(glmmLasso)
require(lattice)
require(doParallel)  #only supports multi-core parallelization
require(foreach)

options("na.action")


#specify number of cores req
cl=makeCluster(10) 
registerDoParallel(cl)

#Load feature matrix
load(file="datamat.RData",verbose=T)

##### Fit a base model (Intercept model, i.e. One-way ANOVA)
#mod_base=glmmPQL(totalRevenue ~ 1, random=~1|storeID,family="gaussian",data=tstMat)
#mod_base

#provide some initial guesstimates of model coeffients (overall intercept, 71 fixed variables, 1120 storeID random effects)
#Delta.start=c(as.numeric(mod_base$coef$fixed),rep(0,10),as.numeric(t(mod_base$coef$random$storeID)))
#Q.start=as.numeric(VarCorr(mod_base)[1,1])  #get fixed intercept variance
#start=Delta.start
####################

#### Fit Intercept as outcome  model
#Formulate model
varString=paste(colnames(trnMat)[1:70],collapse=" + ")
formulaString=paste0("totalRevenue ~ timeOrder + ",varString)
formulaString

#select 10% stores for tuning lambda
tuneStore=sample(unique(trnMat$storeID),0.05*1120)
tuneMat=trnMat[trnMat$storeID %in% tuneStore,]
tuneMat=tuneMat[order(tuneMat$storeID,tuneMat$timeOrder),]
tuneMat[1:5,70:ncol(tuneMat)]
dim(tuneMat)

#possible lambda values for tuning model (start with large lambda penalty)
lambda=seq(500,0,len=2)

mod_glmmLasso=list()

t0=Sys.time()
mod_glmmLasso=foreach(j=1:length(lambda),.packages="glmmLasso") %dopar%{
#for(j in 1:length(lambda)){
  print(paste("Iteration ",j,sep=""))
  tryObj <- try(glmmLasso(totalRevenue ~ timeOrder + retail + medical + others + population
                          + Uninsured + Unemployment + Income.inequality + Median.household.income + competDensity 
                          + Premature.death + Poor.or.fair.health + Low.birthweight + Adult.smoking + Adult.obesity
                          + Food.environment.index + Physical.inactivity + Excessive.drinking 
                          + Sexually.transmitted.infections + Teen.births 
                          + Primary.care.physicians + Dentists + Mental.health.providers 
                          + Preventable.hospital.stays + Diabetic.screening 
                          + Some.college + Children.in.single.parent.households
                          + Severe.housing.problems + Long.commute...driving.alone
                          + Diabetes + HIV.prevalence.rate
                          + Child.mortality + Food.insecurity 
                          + Health.care.costs + Could.not.see.doctor.due.to.cost 
                          + nBiz, rnd=list(storeID=~timeOrder),
                          data=tuneMat, lambda=lambda[j]))
  if(class(tryObj)!="try-error") tryObj else NULL
}
Sys.time() - t0

#remember to terminate the cores when done
stopCluster(cl)


#BIC_vec=sapply(mod_glmmLasso,`[`,"BIC")
#bestLambda=lambda[which.min(BIC_vec)]

save(mod_glmmLasso,tuneStore, file="glmmLasso.RData")







