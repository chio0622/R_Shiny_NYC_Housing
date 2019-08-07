#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(ggiraph)
library(sp)
library(rgdal)

#source('global.R', local = TRUE)

# Define server logic required to draw a histogram
shinyServer(function(input, output) { 
  
  output$map = renderLeaflet({
    leaflet(latlong) %>% 
      addTiles() %>%
      addMarkers(popup= shpf$borough)
  })
  
  
  electric.rate_reactive = reactive({
    dyUnzoom <-function(dygraph) {
      dyPlugin(
        dygraph = dygraph,
        name = "Unzoom",
        path = system.file("plugins/unzoom.js", package = "dygraphs")
      )}
    df <- ddply(Emaster,. (DateTime, Borough),summarise, total.charges=sum(Current.Charges))
    df <- na.omit(df)
    df.3 <- df
    df.3$Month <-month(df$DateTime)
    df.3$Year <- year(df$DateTime)
    new_df <- df.3 %>% filter(Borough == input$borough_1) 
    new_df$Borough <- NULL
    new_df$DateTime <- NULL
    new_df %>% spread(key=Year, value =total.charges, fill = 0)
  })
  
 
  output$electric <- renderDygraph({
    dygraph(electric.rate_reactive()[,c('Month',input$year_1)]) %>% dyRangeSelector(height = 20, strokeColor = "") %>%
      dyAxis("y", label = "Total Cost ($)") %>% dyAxis("x", label="Month (Jan - Dec)",valueRange = c(0, 13)) %>%
      dyLegend(width = 150) %>%dyOptions(labelsKMB = TRUE,axisLineWidth = 1.5)%>% dyUnzoom() %>%dyCrosshair(direction = "vertical")
  })
  
  electric.rate_reactive2 <- reactive({
    kwh = ddply(Emaster,. (DateTime, Borough),summarise, total.KWH=sum(Consumption.KWH))
    kwh$Month <-month(kwh$DateTime)
    kwh$Year <- year(kwh$DateTime)
    kwh_2 = kwh %>% filter(Borough == input$borough_1) 
    kwh_2$Borough <- NULL
    kwh_2$DateTime <- NULL
    kwh_2 %>% spread(key=Year, value =total.KWH, fill = 0)
  })
  
  output$electric_2 <- renderDygraph({
    dygraph(electric.rate_reactive2()[,c('Month',input$year_1)]) %>% dyRangeSelector(height = 20, strokeColor = "") %>%
      dyAxis("y", label = "Total KWH") %>% dyAxis("x", label="Month (Jan - Dec)",valueRange = c(0, 13)) %>%
      dyLegend(width = 150) %>%dyOptions(labelsKMB = TRUE,axisLineWidth = 1.5) %>% dyUnzoom() %>%dyCrosshair(direction = "vertical")
    
  })
  
  electric.scatter <- reactive({
    Emaster2 <- Emaster
    colnames(Emaster2)[4] <- "Total Cost ($)"
    colnames(Emaster2)[5] <- "Kilowatt Hour"
    Emaster2$Year <- as.character(Emaster2$Year)
    Emaster2 %>% filter(Borough == input$borough_1, Year == input$year_1)
    
   
  })
  
  output$scatters <- renderggiraph({
    g <- ggplot(electric.scatter(), aes( x =`Kilowatt Hour`, y =`Total Cost ($)`, color=Year ))
    my_gg <- g + geom_point_interactive(
      aes(tooltip = Year, data_id = Year), size = 2) 
    girafe(code = print(my_gg))
    
  })
  
  water.rate_reactive = reactive({
    dyUnzoom <-function(dygraph) {
      dyPlugin(
        dygraph = dygraph,
        name = "Unzoom",
        path = system.file("plugins/unzoom.js", package = "dygraphs")
      )}
    wf <- ddply(Wmaster,. (DateTime, Borough),summarise, total.charges=sum(Current.Charges))
    wf <- na.omit(wf)
    wf$Month <-month(wf$DateTime)
    wf$Year <- year(wf$DateTime)
    new_wf = wf %>% filter(Borough == input$borough_2) 
    new_wf$Borough <- NULL
    new_wf$DateTime <- NULL
    new_wf2 = new_wf %>% spread(key=Year, value =total.charges, fill = 0)
  })
  
  
  output$water <- renderDygraph({
    dygraph(water.rate_reactive()[,c('Month',input$year_2)]) %>% dyRangeSelector(height = 20, strokeColor = "") %>%
      dyAxis("y", label = "Total Cost ($)") %>% dyAxis("x", label="Month (Jan - Dec)",valueRange = c(0, 13)) %>%
      dyLegend(width = 150) %>%dyOptions(labelsKMB = TRUE,axisLineWidth = 1.5)%>% dyUnzoom() %>%dyCrosshair(direction = "vertical")
  })
  
  water.rate_reactive2 <- reactive({
    hcf = ddply(Wmaster,. (DateTime, Borough),summarise, total.hcf=sum(Total.HCF))
    hcf$Month <-month(hcf$DateTime)
    hcf$Year <- year(hcf$DateTime)
    hcf_2 = hcf %>% filter(Borough == input$borough_2) 
    hcf_2$Borough <- NULL
    hcf_2$DateTime <- NULL
    hcf_3 = hcf_2 %>% spread(key=Year, value =total.hcf, fill = 0)
  })
  
  output$water_2 <- renderDygraph({
    dygraph(water.rate_reactive2()[,c('Month',input$year_2)]) %>% dyRangeSelector(height = 20, strokeColor = "") %>%
      dyAxis("y", label = "Water Consumptionin Hundred Cubic Feet") %>% dyAxis("x", label="Month (Jan - Dec)",valueRange = c(0, 13)) %>%
      dyLegend(width = 150) %>%dyOptions(labelsKMB = TRUE,axisLineWidth = 1.5) %>% dyUnzoom() %>%dyCrosshair(direction = "vertical")
    
  })
  water.scatter <- reactive({
    Wmaster2 <- Wmaster
    colnames(Wmaster2)[4] <- "Total Cost ($)"
    colnames(Wmaster2)[5] <- "Water Consumption in Hundred Cubic Feet"
    Wmaster2$Year <- as.character(Wmaster2$Year)
    Wmaster2 %>% filter(Borough == input$borough_2, Year == input$year_2)
    
    
  })
  
  output$scatters_2 <- renderggiraph({
    g <- ggplot(water.scatter(), aes( x =`Water Consumption in Hundred Cubic Feet`, y =`Total Cost ($)`, color=Year ))
    my_gg <- g + geom_point_interactive(
      aes(tooltip = Year, data_id = Year), size = 2) 
    girafe(code = print(my_gg))
  })
    
    heat.rate_reactive = reactive({
      dyUnzoom <-function(dygraph) {
        dyPlugin(
          dygraph = dygraph,
          name = "Unzoom",
          path = system.file("plugins/unzoom.js", package = "dygraphs")
        )}
      hf <- ddply(Hmaster,. (DateTime, Borough),summarise, total.charges=sum(Current.Charges))
      hf <- na.omit(hf)
      hf$Month <-month(hf$DateTime)
      hf$Year <- year(hf$DateTime)
      new_hf = hf %>% filter(Borough == input$borough_3) 
      new_hf$Borough <- NULL
      new_hf$DateTime <- NULL
      new_hf %>% spread(key=Year, value =total.charges, fill = 0)
      
    })
    
    
    output$heat <- renderDygraph({
      dygraph(heat.rate_reactive()[,c('Month',input$year_3)]) %>% dyRangeSelector(height = 20, strokeColor = "") %>%
        dyAxis("y", label = "Total Cost ($)") %>% dyAxis("x", label="Month (Jan - Dec)",valueRange = c(0, 13)) %>%
        dyLegend(width = 150) %>%dyOptions(labelsKMB = TRUE,axisLineWidth = 1.5)%>% dyUnzoom() %>%dyCrosshair(direction = "vertical")
    })
    
    heat.rate_reactive2 <- reactive({
      therm = ddply(Hmaster,. (DateTime, Borough),summarise, total.therm=sum(Total.Therm))
      therm$Month <-month(therm$DateTime)
      therm$Year <- year(therm$DateTime)
      therm_2 = therm %>% filter(Borough == input$borough_3) 
      therm_2$Borough <- NULL
      therm_2$DateTime <- NULL
      therm_2 %>% spread(key=Year, value =total.therm, fill = 0)
    })
    
    output$heat_2 <- renderDygraph({
      dygraph(heat.rate_reactive2()[,c('Month',input$year_3)]) %>% dyRangeSelector(height = 20, strokeColor = "") %>%
        dyAxis("y", label = "Total Therm Consumption") %>% dyAxis("x", label="Month (Jan - Dec)",valueRange = c(0, 13)) %>%
        dyLegend(width = 150) %>%dyOptions(labelsKMB = TRUE,axisLineWidth = 1.5) %>% dyUnzoom() %>%dyCrosshair(direction = "vertical")
      
    })
    
    heat.scatter <- reactive({
      Hmaster2 <- Hmaster
      colnames(Hmaster2)[4] <- "Total Cost ($)"
      colnames(Hmaster2)[5] <- "Total Therm Consumption"
      Hmaster2$Year <- as.character(Hmaster2$Year)
      Hmaster2 %>% filter(Borough == input$borough_3, Year == input$year_3)
      
      
    })
    
    output$scatters_3 <- renderggiraph({
      g <- ggplot(heat.scatter(), aes( x =`Total Therm Consumption`, y =`Total Cost ($)`, color=Year ))
      my_gg <- g + geom_point_interactive(
        aes(tooltip = Year, data_id = Year), size = 2) 
      girafe(code = print(my_gg))
    })
  
  })



