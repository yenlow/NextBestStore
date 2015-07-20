#Plot maps for slides
#
# 30-Jun-15 Yen Low

#set paths
RScriptPath="/mnt/hgfs/scripts"
ProjectPath="/mnt/hgfs/projects"
setwd(paste(ProjectPath,"/insight/models/maps",sep=""))
getwd()

#load dependencies
require(reshape2)
require(maps)
#require(ggmap)
require(data.table)

source(paste(RScriptPath,"/R/utils.R",sep=""))

#Load map data
load(file="DTmap.RData", verbose=T)

#get lat and long
tmp=strsplit(DTmap$loc,":")
DTmap$lat=as.numeric(sapply(tmp,`[`,1))
DTmap$long=as.numeric(sapply(tmp,`[`,2))
#DTmap[!is.na(DTmap$lat),.(loc,lat,long)]

key(DTmap)

ourStores=as.data.frame(DTmap[!is.na(DTmap$storeID),.(long,lat,storeID,zip)])
predStores=as.data.frame(DTmap[is.na(DTmap$storeID),.(long,lat,storeID,zip)])

ourHighRevStores=subset(DTmap[order(-annRev)],!is.na(DTmap$storeID))[1:100,]
predHighRevStores=subset(DTmap[order(-annRev)],is.na(DTmap$storeID))[1:100,]

ourfastGrowthStores=subset(DTmap[order(-aveQtrGrowth)],!is.na(DTmap$storeID))[1:100,]
predfastGrowthStores=subset(DTmap[order(-aveQtrGrowth)],is.na(DTmap$storeID))[1:100,]

map("usa")
points(ourStores$long,ourStores$lat,pch=16,cex=0.2)


#Get base map
# basemap=get_map(location='US', zoom=4)
basemap=get_map(location='US',maptype="toner-background",zoom=4)

#map our stores (red) and competitor stores (blue)
m= ggmap(basemap)
m= m + geom_point(aes(x=long, y=lat),data=predStores,alpha=0.1,size=1,colour="blue")
m= m + geom_point(aes(x=long, y=lat),data=ourStores,alpha=1,size=1,colour="red")
plot(m)


#map our highRev stores and highPredRev stores
m= ggmap(basemap)
m= m + geom_point(aes(x=long, y=lat,size=annRev),data=predHighRevStores,alpha=0.5,colour="blue")
m= m + geom_point(aes(x=long, y=lat,size=annRev),data=ourHighRevStores,alpha=0.9,colour="red")
m= m + theme(legend.position="none")
plot(m)

#map our fastGrowth stores and fastPredGrowth stores
m= ggmap(basemap)
m= m + geom_point(aes(x=long, y=lat,size=aveQtrGrowth),data=predfastGrowthStores,alpha=0.5,colour="blue")
m= m + geom_point(aes(x=long, y=lat,size=aveQtrGrowth),data=ourfastGrowthStores,alpha=0.5,colour="red")
m= m + theme(legend.position="none")
plot(m)


