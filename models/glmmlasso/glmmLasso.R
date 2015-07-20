# Run glmmLasso of all variables on AWS
# 18-Jun-15 Yen Low


RScriptPath="/mnt/hgfs/scripts"
ProjectPath="/mnt/hgfs/projects"
setwd(paste(ProjectPath,"/insight/models/glmmlasso",sep=""))
getwd()

source(paste(RScriptPath,"/R/utils.R",sep=""))

require(glmmLasso)
require(lattice)

options("na.action")

#Load feature matrix
load(file="datamat.RData",verbose=T)

#Formulate model
varString=paste(colnames(tstMat)[1:70],collapse=" + ")
formulaString=paste0("totalRevenue ~ timeOrder + ",varString)
formulaString

#possible lambda values for tuning model
lambda=seq(500,0,by=-150)  
BIC_vec=c()
mod_glmmLasso=list()

t0=Sys.time()
for(j in 1:length(lambda)){
  print(paste("Iteration ",j,sep=""))
  tryObj <- try(glmmLasso(as.formula(formulaString), rnd=list(storeID=~timeOrder),
                          data=tstMat, lambda=lambda[j]))
  if(class(tryObj)!="try-error") mod_glmmLasso[[as.character(j)]]=tryObj else mod_glmmLasso[[as.character(j)]]=NULL
}
Sys.time() - t0

save(mod_glmmLasso,file="glmmLasso.RData")
