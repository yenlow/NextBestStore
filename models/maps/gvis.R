#set paths
RScriptPath="/mnt/hgfs/scripts"
ProjectPath="/mnt/hgfs/projects"
setwd(paste(ProjectPath,"/insight/models/maps",sep=""))
getwd()

#load dependencies
require(shiny)
require(reshape2)
require(ggplot2)
suppressPackageStartupMessages(require(googleVis))
op=options(gvis.plot.tag=NULL)

source(paste(RScriptPath,"/R/utils.R",sep=""))

#Load map data
load(file="mapData.RData", verbose=T)

top50=dsMap[order(dsMap$aveQtrGrowth,decreasing=T)[1:50],]
bottom50=dsMap[order(dsMap$aveQtrGrowth,decreasing=F)[1:50],]

shinyServer(function(input, output) {
  #allow user to select datasets (bestStores or worstStores)
  datasetInput <- reactive({
    switch(input$dataset,
           "Highest 50 zip codes by quarterly growth" = top50,
           "Lowest 50 zip codes by quarterly growth" = bottom50)
  })
  
  output$view <- renderGvis({
    #lat,long is not exact
    gvisMap(datasetInput(),locationvar="loc",tipvar="name",chartid="yenmap_storeloc180615",
            options=list(region="US",mapType="normal",zoomLevel=4,
                         width=1000, height=600,
                         #                                colors="{colors:['#0066FF', '#CC0000']}",
                         enableScrollWheel=TRUE,showLine=F,useMapTypeControl=TRUE)) 
  })
})




#### Create googlevis objects
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
colOrder=c("name","aveMthGrowth","aveQtrGrowth","aveYrGrowth","population","otherStores","CompetitionDensity","StoreType","UninsuredRate","UnemploymentRate",colnames(factorScores))
m_tab=gvisTable(dsMap[,colOrder], 
                options=list(showRowNumber=T, frozenColumns=1,
                             width="100%",page="enable"))
#plot(m_tab)

#merged map(m_store) with table
m_store_c_timeSeries_m_tab=gvisMerge(m_store_c_timeSeries,m_tab, horizontal=F)
plot(m_store_c_timeSeries_m_tab)


save(m_store_c_timeSeries_m_tab, m_store, c_timeSeries, m_tab, file="mapObj.RData")



######## other plots
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


