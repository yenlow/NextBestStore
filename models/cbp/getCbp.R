#Get County Business Patterns 2013 from http://www.census.gov/econ/cbp/download/
#Files downloaded and stored in cbpDataPath
#
#NCAIS codes downloaded from http://www.census.gov/cgi-bin/sssd/naics/naicsrch?chart=2012

RScriptPath="/mnt/hgfs/scripts"
ProjectPath="/mnt/hgfs/projects"
setwd(paste(ProjectPath,"/insight/models/cbp",sep=""))
getwd()

source(paste(RScriptPath,"/R/utils.R",sep=""))
#source(paste(RScriptPath,"/R/localMySQL.R",sep=""))
#source(paste(RScriptPath,"/R/hampel.R",sep=""))
#source(paste(RScriptPath,"/R/growth.R",sep=""))

#require(RMySQL)
require(reshape2)
require(XLConnect)

#get cpb folder path and then view files
cbpDataPath=paste0(ProjectPath,"/insight/data/cbp2013/")
list.files(cbpDataPath)

#read in zbp13totals.txt for totals aggregated at the zip level (each row is a zip)
cbpDf=read.table(paste0(cbpDataPath,"zbp13totals.txt"),sep=",",header=T,
                 colClasses=c("character",rep(NA,12)))
cbpDf=cbpDf[,c("zip","city","emp","ap","est")]
colnames(cbpDf)=c("zip","city","nEmployed","AnnPayrollK","nBiz")
dim(cbpDf)

#read in zbp13detail.txt for bizType (i.e. ncais code) per row (company)
bizTypeDf=read.table(paste0(cbpDataPath,"zbp13detail.txt"),sep=",",header=T,
                 colClasses=c("character",rep(NA,11)))
bizTypeDf=bizTypeDf[,c("zip","naics","est")]
colnames(bizTypeDf)=c("zip","naics","nBiz")
bizTypeDf=read.table(paste0(cbpDataPath,"zbp13detail.txt"),sep=",",header=T,
                     colClasses=c("character",rep(NA,11)))
#read NCAIS codes
#downloaded from http://www.census.gov/cgi-bin/sssd/naics/naicsrch?chart=2012
wb = loadWorkbook(paste0(cbpDataPath,"2-digit_2012_Codes.xls"))
df = readWorksheet(wb, sheet="tbl_2012_title_description_coun", header=TRUE)

#view all naics codes
unique(bizTypeDf$naics)

#Get the higher level NAICS codes
#bizTypeDf$naics[grep("-+$",bizTypeDf$naics)]  #truncate to first 2 
#62=health industry
wantedID=union(grep("-+$",bizTypeDf$naics),grep("^62",bizTypeDf$naics)) #truncate to first 4
relTypeDf=bizTypeDf[wantedID,]

#get selected NACIS (2 level codes and 4 level for code 62, healthcare)
selectedNAICS=unique(relTypeDf$naics)
relTypeDf$naics=substr(relTypeDf$naics,1,4)
relTypeDf$naics=gsub("--$","",relTypeDf$naics)
relTypeDf$naics=gsub("^-+","overall",relTypeDf$naics)
unique(relTypeDf$naics)
head(relTypeDf)
cbpTypeAsFeatures=dcast(relTypeDf, zip ~ naics, sum, na.rm=T)

#TODO: map NAICS to names
indNames=df$Description[match(colnames(cbpTypeAsFeatures),df$Code)]
sum(is.na(indNames))
#manually map unmapped codes
indNames[is.na(indNames)]=c("zip","manufacturing","retail","transport_warehousing","unclassified","overall")
sum(is.na(indNames))
indNames=gsub("[[:punct:]]","",indNames)
indNames=gsub("[[:blank:]]","_",indNames)
indNames
colnames(cbpTypeAsFeatures)=indNames

cbpDf=merge(cbpDf,cbpTypeAsFeatures,by="zip",all.x=T)

save(cbpDf,bizTypeDf,relTypeDf,cbpTypeAsFeatures,file=paste0(ProjectPath,"/insight/data/cbp.RData"))
#load(file=paste0(ProjectPath,"/insight/data/cbp.RData"),verbose=T)
