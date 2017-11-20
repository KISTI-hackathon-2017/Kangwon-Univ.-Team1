library(sp)
library(ggplot2)
library(rgeos)
library(maptools)
library(shiny)
library(leaflet)
library(maptools)
library(TTR)
library(shinythemes)
library(forecast)


daejeon_tashu<-read.csv("tashu_report.csv",header=TRUE)
tashu_final_usage<-read.csv("tashu_final_usage.csv",header=TRUE)
daejeon_seoGu_3<-read.csv("daejeon_seo-gu_3.csv",header=TRUE)
daejeon_yuseongGu_31<-read.csv("daejeon_yuseong-gu_31.csv",header=TRUE)
daejeon_09_12 <- readShapePoly("daejeon_09_12")
daejeon_12_15 <- readShapePoly("daejeon_12_15")
daejeon_15_18 <- readShapePoly("daejeon_15_18")
daejeon_total <- readShapePoly("daejeon_total")
cyc <- colorRampPalette(c("blue","red"), space = "rgb")

daejeon_seoGu_3.ts <- ts(daejeon_seoGu_3[6], st=1, end=19, fr=19)
daejeon_yuseongGu_31.ts <- ts(daejeon_yuseongGu_31[6], st=1, end=19, fr=19)

icon("calendar") # standard icon
icon("calendar", "fa-10x") # 3x normal size
icon("cog", lib = "glyphicon") # From glyphicon library

ui <- tagList(
  navbarPage(theme = shinytheme("united"),
    "Situation board",tabPanel("Daejeon",
                               sidebarPanel(
                                 selectInput("column", label = h3("Index:"),
                                             choices = list("pm10" = "mean_pm10_",
                                                            "NH3" = "mean_nh3",
                                                            "CO" = "mean_co",
                                                            "NO2" = "mean_no2",
                                                            "VOC" = "mean_voc"),
                                             selected = "mean_pm10_"),
                                 selectInput("Gu", label = h3("Gu:"),
                                             choices = list("seo-gu"="1","jung-gu"="2","dong-gu"="3", "yuseong-gu"="4","daedeog-gu"="5"),
                                             selected = "1"),
                                 radioButtons("radio1", label = h3("Slot:"),
                                              choices = list("09~12" = 1,"12~15" = 2, "15~18" = 3, "total" = 4),
                                              selected = 4)
                                 , width=2
                               ),
                               mainPanel(
                                 tabsetPanel(
                                   tabPanel("Map",icon = icon("map"),
                                            fluidRow(
                                              column(align="center", 6, wellPanel(
                                                h4("Gu_MAP"),
                                                plotOutput("Gu_map")
                                                #Gu
                                              )),
                                              
                                              column(align="center", 6, wellPanel(
                                                h4("Daejeon_MAP"),
                                                plotOutput("All_map")
                                                #total
                                              ))
                                            )
                                   ),
                                   tabPanel("Histogram",icon = icon("bar-chart-o"),
                                            fluidRow(
                                              column(align="center", 6, wellPanel(
                                                h4("Gu sensing frequency"),
                                                plotOutput("Gu_hist")
                                                #Gu
                                              )),
                                              
                                              column(align="center", 6, wellPanel(
                                                h4("Daejeon sensing frequency"),
                                                plotOutput("All_hist")
                                                #total
                                              ))
                                            )
                                   ),
                                   tabPanel("Scatter_Plot", icon=icon("circle"),fluidRow(
                                     column(align="center", 6, wellPanel(
                                       h4("Gu sensing Data and public bike usage amount"),
                                       plotOutput("Gu_Scatter"),
                                       h4("Correlation analysis")
                                       #Gu
                                     )),
                                     
                                     column(align="center", 6, wellPanel(
                                      h4("Daejeon sensing Data and public bike usage amount"),
                                       plotOutput("All_Scatter"),
                                       h4("Correlation analysis")
                                       #total
                                     ))
                                   )),
                                   tabPanel("Line_Graph",icon=icon("line-chart"),
                                            selectInput("Gu_trendsIdx", label = h4("Station:"),
                                                        choices = list("station_3(seo-gu)" = "station_3(seo-gu)",
                                                                       "station_31(yuseong-gu)"= "station_31(yuseong-gu)"), selected = "station_3(seo-gu)"),
                                            plotOutput("Gu_Trends")
                                            )
                                   ,tabPanel("Report", icon=icon("bicycle"),leafletOutput("daejeonmap",width="100%", height = 550))
                                 ), width = 8
                               )
    )
    ,
    tabPanel("Busan")
  )
)
server <- function(input, output) {
  output$Gu_map <- renderPlot({
    if(input$radio1==1){
      xx <- daejeon_09_12
    } else if(input$radio1==2){
      xx <- daejeon_12_15
    } else if(input$radio1==3){
      xx <- daejeon_15_18
    } else {
      xx <- daejeon_total
    }
    if(input$Gu=="1"){
      xx<-as(xx, "SpatialPolygonsDataFrame")[xx$gu=="1",]
    } else if(input$Gu=="2"){
      xx<-as(xx, "SpatialPolygonsDataFrame")[xx$gu=="2",]
    } else if(input$Gu=="3"){
      xx<-as(xx, "SpatialPolygonsDataFrame")[xx$gu=="3",]
    } else if(input$Gu=="4"){
      xx<-as(xx, "SpatialPolygonsDataFrame")[xx$gu=="4",]
    } else {
      xx<-as(xx, "SpatialPolygonsDataFrame")[xx$gu=="5",]
    }
    spplot(xx, zcol = input$column, col.regions = cyc(95), sp.layout = daejeon_total,xlim=daejeon_total@bbox[1,],ylim=daejeon_total@bbox[2,])
  })
  
  output$All_map <- renderPlot({
    if(input$radio1==1){
      xx <- daejeon_09_12
    } else if(input$radio1==2){
      xx <- daejeon_12_15
    } else if(input$radio1==3){
      xx <- daejeon_15_18
    } else {
      xx <- daejeon_total
    }
    spplot(xx, zcol = input$column ,col.regions = cyc(95), sp.layout = daejeon_total,xlim=daejeon_total@bbox[1,],ylim=daejeon_total@bbox[2,])
  })
  
  output$All_hist <- renderPlot({
    if(input$column=="mean_pm10_"){
      idxName="pm10"
    }else if(input$column=="mean_co"){
      idxName="co" 
    }else if(input$column=="mean_nh3"){
      idxName="nh3"
    }else if(input$column=="mean_voc"){
      idxName="voc"
    }else{
      idxName="no2"
    }
    
    if(input$radio1==1){
      xx <- daejeon_09_12
    } else if(input$radio1==2){
      xx <- daejeon_12_15
    } else if(input$radio1==3){
      xx <- daejeon_15_18
    } else {
      xx <- daejeon_total
    }
    xx<-as(xx, "data.frame")[]
    hist(xx[,input$column],xlab = idxName,main = NULL, col = "orangered")
  })
  
  output$Gu_hist <- renderPlot({
    if(input$column=="mean_pm10_"){
      idxName="pm10"
    }else if(input$column=="mean_co"){
      idxName="co" 
    }else if(input$column=="mean_nh3"){
      idxName="nh3"
    }else if(input$column=="mean_voc"){
      idxName="voc"
    }else{
      idxName="no2"
    }
    
    if(input$radio1==1){
      xx <- daejeon_09_12
    } else if(input$radio1==2){
      xx <- daejeon_12_15
    } else if(input$radio1==3){
      xx <- daejeon_15_18
    } else {
      xx <- daejeon_total
    }
    if(input$Gu=="1"){
      xx<-as(xx, "data.frame")[xx$gu=="1",]
    } else if(input$Gu=="2"){
      xx<-as(xx, "data.frame")[xx$gu=="2",]
    } else if(input$Gu=="3"){
      xx<-as(xx, "data.frame")[xx$gu=="3",]
    } else if(input$Gu=="4"){
      xx<-as(xx, "data.frame")[xx$gu=="4",]
    } else {
      xx<-as(xx, "data.frame")[xx$gu=="5",]
    }
    hist(xx[,input$column],xlab = idxName, main = NULL, col = "orangered")
  })
  
  output$Gu_Scatter <- renderPlot({
    if(input$Gu=="1"){
      idxName2="seo-gu"
    }else if(input$Gu=="2"){
      idxName2="jung-gu" 
    }else if(input$Gu=="3"){
      idxName2="dong-gu"
    }else if(input$Gu=="4"){
      idxName2="yuseong-gu"
    }else{
      idxName2="daedeog-gu"
    }
    xx<-tashu_final_usage[tashu_final_usage$gu==idxName2 & tashu_final_usage$time_type==as.numeric(input$radio1),]
    if(input$column=="mean_pm10_"){
      idxName="pm10"
    }else if(input$column=="mean_co"){
      idxName="co" 
    }else if(input$column=="mean_nh3"){
      idxName="nh3"
    }else if(input$column=="mean_voc"){
      idxName="voc"
    }else{
      idxName="no2"
    }
    plot(xx$usage,xx[,idxName],xlab="AMT",ylab=idxName)
    abline(lm(xx[,idxName] ~ xx$usage),col="orangered")
  })
  
  output$All_Scatter <- renderPlot({
    xx<-tashu_final_usage[tashu_final_usage$time_type==as.numeric(input$radio1),]
    if(input$column=="mean_pm10_"){
      idxName="pm10"
    }else if(input$column=="mean_co"){
      idxName="co" 
    }else if(input$column=="mean_nh3"){
      idxName="nh3"
    }else if(input$column=="mean_voc"){
      idxName="voc"
    }else{
      idxName="no2"
    }
    plot(xx$usage,xx[,idxName],xlab="AMT",ylab=idxName)
    abline(lm(xx[,idxName] ~ xx$usage),col="orangered")
  })
  
  output$Gu_Trends <- renderPlot({
    if(input$Gu_trendsIdx=="station_3(seo-gu)"){
      plot.ts(daejeon_seoGu_3.ts, main="Trends", ylab = "usage of the Station")
    } else if(input$Gu_trendsIdx=="station_31(yuseong-gu)"){
      plot.ts(daejeon_yuseongGu_31.ts, main="Trends", ylab = "usage of the Station")
    }
  })
  
  getColor <- function(daejeon_tashu) {
    sapply(daejeon_tashu$rate2, function(rate2) {
      if(rate2 == 1) {
        "green"
      } else if(rate2 == 2) {
        "orange"
      } else {
        "red"
      } })
  }
  
  icons <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = getColor(daejeon_tashu)
  )
  
  daejeonmap<-leaflet(daejeon_tashu) %>% setView(lng=127.384538, lat=36.350616, zoom = 13)
  daejeonmap <- daejeonmap %>% addProviderTiles(providers$Esri.WorldStreetMap)%>%
    addAwesomeMarkers(~lng, ~lat, icon = icons, popup = ~as.character(no), label = ~as.character(no))%>%
    addMiniMap(
      tiles = providers$Esri.WorldStreetMap,
      toggleDisplay = TRUE)
  
  output$daejeonmap = renderLeaflet(daejeonmap)
}

shinyApp(ui=ui, server = server)
