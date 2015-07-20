RScriptPath="/mnt/hgfs/scripts"
ProjectPath="/mnt/hgfs/projects"
setwd(paste(ProjectPath,"/insight/models/zip",sep=""))
getwd()

require(choroplethrZip) 

storeZip=read.table("../../data/mckesson/stores.csv",sep=",",header=T,fill=NA,colClasses="character")
colnames(storeZip)
storesByState=as.data.frame(table(storeZip[,c("ACCT_DLVRY_ST_ABRV")]))
colnames(storesByState)=c("state","numStores")
storesByZip=as.data.frame(table(storeZip[,c("ACCT_DLVRY_ZIP")]))
colnames(storesByZip)=c("zip","numStores")

#CT=06475, oakland="94306"
storesByZip[storesByZip[,1]=="06475",]

storesByZip$region=as.character(storesByZip$region)

#plot using choroplethrZip (not interactive)
#data(df_pop_zip)   #load test data
data(zip.regions)
zipCA=zip.regions$region[zip.regions$state.name=="california"]
zip_choropleth(storesByZip,zip_zoom=zipCA)

#plot with googleViz and put on Shiny
m=gvisGeoChart(storesByState,
             locationvar="state", colorvar="numStores",
             options=list(region="US", displayMode="regions", 
                          resolution="provinces",
                          width=1000, height=600,
                          colorAxis="{colors:['#FFFFFF', '#0000FF']}"
             )) 
plot(m)


m=gvisGeoChart(storesByZip,
               locationvar="zip", colorvar="numStores",
               options=list(region="US", displayMode="regions", 
                            resolution="provinces",
                            width=1000, height=600,
                            colorAxis="{colors:['#FFFFFF', '#0000FF']}"
               )) 
plot(m)



shinyServer(function(input, output) {
  myYear <- reactive({
    input$Year
  })
  output$year <- renderText({
    paste("Democratic share of the presidential vote in", myYear())
  })
  output$gvis <- renderGvis({
    myData <- subset(dat, 
                     (year > (myYear()-1)) & (year < (myYear()+1)))
    gvisGeoChart(myData,
                 locationvar="state", colorvar="demVote",
                 options=list(region="US", displayMode="regions", 
                              resolution="provinces",
                              width=500, height=400,
                              colorAxis="{colors:['#FFFFFF', '#0000FF']}"
                 ))     
  })
})
