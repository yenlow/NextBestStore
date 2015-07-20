shinyServer(function(input, output) {
  
  #load dependencies
  require(shiny)
  require(shinythemes)
  require(shinydashboard)
  require(DT)
  require(data.table)
  require(htmlwidgets)
  require(sparkline)  #AWS has the latest github version
  suppressPackageStartupMessages(require(googleVis))
  op=options(gvis.plot.tag=NULL)
  
  source("sparkline.R")
  
  #Load map data
  load(file="DTmap.RData", verbose=T)
  load(file="drugsData.RData", verbose=T)
#  load(file="sumpred.RData", verbose=T)     #for gvisLine
  gc()
  
  #select and arrange columns of interest
  colOrder=c("name","revenue","annRev","aveQtrGrowth","population","StoreType","storesPerCapita",
             "UninsuredRate","UnemploymentRate","Uninsured","Natal","PrematureDeath","Commute","HealthCost",
             "Unemployment","Urban","PrCare","TertCare","BizAct","MedicalRxStore","Pollution","HealthScreening","OtherBiz","Alcholism")
  colpretty=c("Location","Revenue","Annual Revenue","Quarterly Growth","Population","Store Type","Stores Per 100,000",
             "%Uninsured","%Unemployed","Uninsured","Natal","PrematureDeath","Commute","HealthCost",
             "Unemployment","Urban","PrCare","TertCare","BizAct","MedicalRxStore","Pollution","HealthScreening","OtherBiz","Alcholism")
  
  #subset new zip areas
  newdat = reactive({
    #subset table (head or tail)
    subsettedData=switch( input$highOrFast,
                          high=head(subset(DTmap[order(-annRev)],is.na(storeID)),input$num), #if by totRev
                          fast=head(subset(DTmap[order(-aveQtrGrowth)],is.na(storeID)),input$num) #if by aveQtrGrowth
                        )
    as.data.frame(subsettedData)
  })
  
  #subset zips with existing stores
  olddat = reactive({
    #subset table (head or tail)
    subsettedData=switch( input$highOrFast,
                          high=head(subset(DTmap[order(-annRev)],!is.na(storeID)),input$num),  #if by totRev
                          fast=head(subset(DTmap[order(-aveQtrGrowth)],!is.na(storeID)),input$num) #if by aveQtrGrowth
                  )
    as.data.frame(subsettedData)
  })

  #generate valuebox to show the potential revenue gains (only for new zips)
  output$gain = renderValueBox({
    revGain=newdat()[,"annRev"]
    fc <- formatC(sum(revGain,na.rm=T), format="fg", big.mark = ",")
    infoBox( "Potential revenue", sprintf("%s", fc),
             icon=icon("usd", lib = "glyphicon"),
             width=NULL, color="yellow",fill=T
    )
  })
  
  #get sales of store in zip area (only for existing stores)
  datChange = reactive({
    changeDf[changeDf$zip==toString(input$zip),c("itemName","volChange","revChange")]
  })
  
  output$zip <- renderGvis({
    gvisComboChart(datChange(),xvar="itemName",yvar=c("volChange","revChange"),
                   options=list(seriesType="bars",orientation='horizontal',width="100%", height="250",
                                series="[ {type:'bars', targetAxisIndex:0,color:'red',visibleInLegend: false}, 
                                          {targetAxisIndex:1,color:'blue',visibleInLegend: false}]",
                                vAxes="[ {title:'Change in volume (units)',
                                          titleTextStyle: {color:'red',fontSize:14,bold:1,italic:0},
                                          baseline:0, baselineColor:'red'},
                                          {title:'Change in revenue ($)',
                                          titleTextStyle: {color:'blue',fontSize:14, bold:1,italic:0}, 
                                          baseline:0, baselineColor:'blue'}]"
                   ))
  })
  
  output$mapTabSelected <- renderText({
    input$mapTab
  })

  #map predicted areas
  output$newmap <- renderGvis({
    #lat,long is not exact
    gvisMap(newdat(),locationvar="loc",tipvar="name",chartid="yenmap_storeloc180615",
            options=list(region="US",mapType="normal",zoomLevel=4,
                         width="100%", height=370,
                         #colors="{colors:['#0066FF', '#CC0000']}",
                         enableScrollWheel=TRUE,showLine=F,useMapTypeControl=TRUE)) 
  })
  
  #map existing store areas
  output$oldmap <- renderGvis({
    #lat,long is not exact
    gvisMap(olddat(),locationvar="loc",tipvar="name",
            options=list(region="US",mapType="normal",zoomLevel=4,
                         width="100%", height=370,
                         #colors="{colors:['#0066FF', '#CC0000']}",
                         enableScrollWheel=TRUE,showLine=F,useMapTypeControl=TRUE)) 
  })
  
  output$comparemap <- renderGvis({
    tmp=newdat()
    tmp$existing=0
    tmp2=rbind(tmp,cbind(olddat(),existing=1))
    gvisGeoChart(tmp2,
                       locationvar="loc",colorvar="existing",sizevar="annRev",hovervar="name",
                       options=list(region="US", displayMode="markers", markerOpacity=0.4,
                                    resolution="provinces",width="100%", height=370,
                                    tooltip.trigger="selection",
                                    magnifyingGlass.zoomFactor=2,enableRegionInteractivity=F,
                                    colorAxis="{colors:['#CC0000', '#0066FF']}")) 
  })


  #output selected areas as a table
  output$table = renderDataTable({
    datSubset=switch( input$mapTab,
                      new=newdat(),
                      old=olddat(),
                      compare=rbind(newdat(),olddat()))
                      
    
    #JS call
    fnDrawCallback = JS("function (oSettings, json) {
                        $('.spark:not(:has(canvas))').sparkline('html', {
                        type: 'line',
                        zeroColor: 'black'
                        });
    }")
    
    #set column where you want the sparkline
    columnDefs = list(list(
      targets = c(1),
      render = JS("function(data, type, full){
                  return '<span class=spark>' + data + '</span>'           
      }")
    ))
    
#    colwidths <- c("20px", "20px", rep("10px",21))
#    col.names <- colpretty
#    aoColumnDefs <- list()
#    for(i in 1:length(col.names)){
#      column <- list(sWidth=colwidths[i], sTitle=col.names[i], aTargets=list(i-1))
#      aoColumnDefs[[i]] <- column
#    }
#    
#    aoColumnDefs[[2]]$aRender=JS("function(data, type, full){
#                  return '<span class=spark>' + data + '</span>'           
#      }")
    
    #make datatable
    d1 <- datatable(datSubset[,colOrder],rownames=F,filter="none",
#                    caption="Location characteristics",
                    style="bootstrap",
                    colnames=colpretty,
                    ,options = list(bautoWidth=T,
                      columnDefs = columnDefs,
                      fnDrawCallback = fnDrawCallback,
                      destroy = TRUE, scrollX=T,
                      dom='C<"clear">lfrtip',
                      colVis=list(exclude=c(0:5))),
                      extensions='ColVis'
    )
#                                                                            ColReorder=NULL,FixedColumns=list(leftColumns=1)    
    #return final table for rendering
    d1
    })
  
  output$legend=renderImage({
    list(src="legend.PNG", contentType="image/png", width=150,height=60)
  }, deleteFile=F)

})  #end of Shiny function

#### LEGACY
# toy example for 1 sparkline
#  output$spark=renderSparkline({
#    sparkline(timeseriescols[,1])
#  })


#   output$gainChart = renderGvis({
#    gvisLineChart(sumPredRev,xvar="ID",yvar="cumsum",
#                            options=list( height="100%",width="100%",
#                                          vAxis="{format:'long',logScale:'TRUE'}",
#                                          hAxis="{title:'Number of Stores',logScale:'TRUE'}",
#                                          legend="{position:'none'}"))
#   })
