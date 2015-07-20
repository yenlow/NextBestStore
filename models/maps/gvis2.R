#set paths
RScriptPath="/mnt/hgfs/scripts"
ProjectPath="/mnt/hgfs/projects"
setwd(paste(ProjectPath,"/insight/models/maps",sep=""))
getwd()

suppressPackageStartupMessages(require(googleVis))
op=options(gvis.plot.tag=NULL)

#get zip to long, lat
require(zipcode)
data(zipcode)

source(paste(RScriptPath,"/R/utils.R",sep=""))
require(reshape2)
require(ggplot2)

#Load data
load(file=paste0(ProjectPath,"/insight/data/dsStore.RData"),verbose=T)  #store level
load(file=paste0(ProjectPath,"/insight/models/fa/FA.RData"),verbose=T)  #factor scores
load(file=paste0(ProjectPath,"/insight/models/glmmlasso_fa/datamat.RData"),verbose=T)  #feature matrix
load(file=paste0(ProjectPath,"/insight/data/storeChar.RData"),verbose=T)     #stores grouped by zip, county, state
load(file=paste0(ProjectPath,"/insight/data/competition.RData"),verbose=T)
load(file=paste0(ProjectPath,"/insight/data/revGrowth.RData"),verbose=T)  #response: sales growthload
rm(storeDf,allStores)
lsos()
gci()


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


#create matrix for map
tmp=merge(competDf[zip_bestStores,],zipcode[,c("zip","city","state","latitude","longitude")],by.x=0  ,by.y="zip")
tmp$best=1
tmp2=merge(competDf[zip_worstStores,],zipcode[,c("zip","city","state","latitude","longitude")],by.x=0  ,by.y="zip")
tmp2$best=0
dsMap=rbind(tmp,tmp2)
dsMap$loc=paste(round(dsMap$latitude,1),round(dsMap$longitude,1),sep=":")
dsMap$name=paste0(dsMap$city, ", " ,dsMap$state," ", as.character(dsMap$Row.names))

#merge with other zip characteristics
dsMap=merge(dsMap,dsStore,by.x="Row.names",by.y="zip",sort=F)
dsMap=merge(dsMap,factorScores,by.x="storeID",by.y=0,sort=F)

#drop repeated colnames
dsMap=dsMap[,-grep("Row.names|latitude|longitude|city|state.y|county|busType|fip|county|compet.+.y$",colnames(dsMap),ignore.case=T)]
#rename colnames
colnames(dsMap)[grep("Row.names|latitude|longitude|city|state.y|county|busType|fip|county|compet.+.y$",colnames(dsMap),ignore.case=T)]
colnames(dsMap)[grep("numCompetitor.x",colnames(dsMap))]="Competitors"
colnames(dsMap)[grep("numClient",colnames(dsMap))]="ClientsInZip"
colnames(dsMap)[grep("competDensity.x",colnames(dsMap))]="CompetitionDensity"
colnames(dsMap)[grep("state.x",colnames(dsMap))]="State"
colnames(dsMap)[grep("bizType",colnames(dsMap))]="StoreType"
colnames(dsMap)[grep("Unemployment.x",colnames(dsMap))]="UnemploymentRate"
colnames(dsMap)[grep("Uninsured.x",colnames(dsMap))]="UninsuredRate"
#Rename factor scores
colnames(dsMap)[grep("Unemployment.y",colnames(dsMap))]="Unemployment"
colnames(dsMap)[grep("Uninsured.y",colnames(dsMap))]="Uninsured"

#colnames(dsMap)[1:5]=c("Competitors","CompetitionDensity","best","Location","Name")

#create google geochart
m_store=gvisGeoChart(dsMap[,c("loc","best","Competitors","name")],
                     locationvar="loc",colorvar="best",sizevar="Competitors",hovervar="name",
                     options=list(region="US", displayMode="markers", markerOpacity=0.4,
                                  resolution="provinces",width=1000, height=600,
                                  tooltip.trigger="selection",
                                  magnifyingGlass.zoomFactor=2,enableRegionInteractivity=F,
                                  colorAxis="{colors:['#0066FF', '#CC0000']}")) 
#plot(m_store)


#create time series plot with google line chart
c_timeSeries <-  gvisLineChart(datamat[datamat$storeID=="855349",c("timeOrder","totalRevenue")],
                        xvar="timeOrder", yvar="totalRevenue",
                        options=list(
                          title="Monthly revenue of our stores in the area",
                          titleTextStyle="{color:'black', 
                                          fontName:'Arial', 
                                           fontSize:16}",                         
                          hAxis="{title:'Month', titleTextStyle:{color:'black'}}",
#                          series="[{color:'red', targetAxisIndex: 0}],
#                                   ,{color: 'red',targetAxisIndex:1}]",
#                         vAxes="[{title:'val1'}, {title:'val2'}]",
                          legend="none",
#                          curveType="function",
                          width=1000,height=600))
#plot(c_timeSeries)

#merge map with time series
m_store_c_timeSeries=gvisMerge(m_store,c_timeSeries, horizontal=T)
#plot(m_store_c_timeSeries)

#generate table
"State"
colOrder=c("name","State","population","Competitors","CompetitionDensity","StoreType","UninsuredRate","UnemploymentRate",colnames(factorScores))
m_tab=gvisTable(dsMap[,colOrder], 
                options=list(showRowNumber=T, frozenColumns=1,
                             width="100%",page="enable"))
plot(m_tab)

#merged map(m_store) with table
m_store_c_timeSeries_m_tab=gvisMerge(m_store_c_timeSeries,m_tab, horizontal=F)
plot(m_store_c_timeSeries_m_tab)


save(dsMap, m_store_c_timeSeries_m_tab, m_store, c_timeSeries, m_tab, file="mapObj.RData")



#View stores in factor space
pairID=t(combn(1:7,2))
png(file="factorSpace.png",width=7,height=7,units="in",res=300,bg="transparent")
par(mfrow=c(4,5),mar=c(4,4,1,1))
for(i in 1:(nrow(pairID)-1)){
  plot(factorScores[rownames(factorScores) %in% bestStores,pairID[i,]],pch=16,col="#CC0000")
  points(factorScores[rownames(factorScores) %in% worstStores,pairID[i,]],pch=16,col="#0066FF")
}
dev.off()

tmp=as.data.frame(factorScores[rownames(factorScores) %in% bestStores,])
tmp$best=1
tmp2=as.data.frame(factorScores[rownames(factorScores) %in% worstStores,])
tmp2$best=2
selFactors=rbind(tmp,tmp2)


#Compare factor scores btwn the fastest and slowest zip areas
png(file="factorScoreComparison.png",width=7,height=7,units="in",res=300,bg="transparent")
par(mar=c(2,10,2,1))
boxplot(tmp[,15:1],at=seq(2,by=5,len=15), horizontal=T,las=1,col="#CC0000",pch=16,cex=0.5,
        main="Fastest vs slowest growing zipcodes")
boxplot(tmp2[,15:1],at=seq(1,by=5,len=15), horizontal=T,las=2,col="#0066FF",add=T,axes=F,pch=16,cex=0.5)
legend("bottomright",c("Slow growing","Fast growing"),pch=15,col=c("#0066FF","#CC0000"))
dev.off()





####Create table for ggplot
colnames(datamat)
wantedCol=c("totalRevenue","timeOrder","storeID")
ggmat=datamat[datamat$storeID %in% bestStores,wantedCol]
ggmat$best=1
tmp=datamat[datamat$storeID %in% worstStores,wantedCol]
tmp$best=0
ggmat=rbind(ggmat,tmp)

#normalize
#ggmat$RevNorm=aggregate(totalRevenue~storeID,data=ggmat,scale)

p = ggplot(ggmat,aes(x=timeOrder,y=totalRevenue,group=storeID,colour=factor(best)))
p = p + geom_line() + scale_y_continuous(limits=c(0,75000))
plot(p)


#lat,long is not exact
m_store2=gvisMap(dsMap,locationvar="Location",tipvar="Name",chartid="yenmap_storeloc180615",
                     options=list(region="US",mapType="normal",zoomLevel=4,
                                  width=1000, height=600,colors="{colors:['#0066FF', '#CC0000']}",
                                  enableScrollWheel=TRUE,showLine=F,useMapTypeControl=TRUE)) 
plot(m_store2)


#### plot state choropleth
#get population
#require(choroplethr)
#data(df_pop_state)
m_choropleth=gvisGeoChart(store_state,
                          locationvar="state", colorvar="Stores",
                          options=list(region="US", displayMode="regions", 
                                       resolution="provinces",
                                       width=1000, height=600,
                                       colorAxis="{colors:['#FFFFFF', '#0000FF']}")) 
plot(m_choropleth)


#Intensity map (must have 2 columns, state code is the abbrev)
store_state$population=df_pop_state$value[-1]
m_intensity=gvisIntensityMap(store_state,locationvar="state",numvar=c("population","Stores"),
                             options=list(region="usa",resolution="provinces"))
plot(m_intensity)


