#Get County Health Rankings 2015 from http://www.countyhealthrankings.org/rankings/data
#Files downloaded and stored in dataPath
#
# 16-Jun-15 Yen Low

RScriptPath="/mnt/hgfs/scripts"
ProjectPath="/mnt/hgfs/projects"
setwd(paste(ProjectPath,"/insight/models/chr",sep=""))
getwd()

source(paste(RScriptPath,"/R/utils.R",sep=""))


#get cpb folder path and then view files
dataPath=paste0(ProjectPath,"/insight/data/chr2015/")
list.files(dataPath)

#read in zbp13totals.txt for totals aggregated at the zip level (each row is a zip)
tmp=read.table(paste0(dataPath,"2015 CHR Analytic Data.csv"),sep=",",header=T,
                 colClasses=c(rep("character",5),rep(NA,329-5)))
dim(tmp)

#keep unranked counties which still have useful info
table(tmp$"County.that.was.not.ranked") 

#subset the columns of interest
colnames(tmp)
colsSel=grep("CODE$|^State$|^County$|^County.that.was.not.ranked$|.+.Value$",colnames(tmp))
colnames(tmp)[colsSel]
chrDf=tmp[,colsSel]

#pad location codes with leading zeros to make 5-digit FIP code
chrDf$STATECODE=formatC(as.integer(chrDf$STATECODE), width=2, flag="0", format="d")
chrDf$COUNTYCODE=formatC(as.integer(chrDf$COUNTYCODE), width=3, flag="0", format="d")
chrDf$fip=paste0(chrDf$STATECODE,chrDf$COUNTYCODE)
head(chrDf)

colnames(chrDf)=gsub(".Value$","",colnames(chrDf))

save(chrDf,file=paste0(ProjectPath,"/insight/data/chr.RData"))

