library(shiny)
library(ggplot2)

load('dtsdata.Rdata')

ui <- fluidPage(
  headerPanel('DTS Monitoring Results'),
  sidebarLayout(
    sidebarPanel(sliderInput(inputId = "location", 
                             label = "Choose a location (m)", 
                             value = 5000, min = 0, max = 60000,step=500),
                 dateInput(inputId = 'start', label = 'Start Date and Time', value = as.POSIXct('2016-09-19'), min = as.POSIXct('2016-09-17'), max  = as.POSIXct('2016-09-30'),
                           format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                           language = "en", width = NULL),
                 sliderInput(inputId = "starttime", 
                             label = "", 
                             value = 6, min = 0, max = 23),
                 
                 dateInput(inputId = 'end', label = 'End Date and Time', value = as.POSIXct('2016-09-21'), min = as.POSIXct('2016-09-17'), max  = as.POSIXct('2016-09-30'),
                           format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                           language = "en", width = NULL)
                 
                 ,
                 
                 sliderInput(inputId = "endtime", 
                             label = "", 
                             value = 21, min = 0, max = 23)),
    mainPanel( plotOutput("loadplot"),
               plotOutput("tempplot"))
  )
  
 
  
)

server <- function(input, output) {
  plotdata <- reactive({
    dts.data$Temperature[which(abs(dts.data$Locations-input$location)==min(abs(dts.data$Locations-input$location))),]
  })
  
  datelimit1 <- reactive({
    
    as.POSIXct(paste(as.character(input$start),paste(input$starttime,':00:00',sep='')))
    
    #c(as.POSIXct(input$start),as.POSIXct(input$end))
    
  })
  datelimit2 <- reactive({
    
    as.POSIXct(paste(as.character(input$end),paste(input$endtime,':00:00',sep='')))
    
    #c(as.POSIXct(input$start),as.POSIXct(input$end))
    
  })
  
  output$tempplot <- renderPlot({
    
    
    
    ggplot(mapping=aes(x=dts.data$Temp.Time, y = plotdata()))+
      geom_line()+
      scale_x_datetime(
        limits = c(datelimit1(),datelimit2()),
        name = 'Time and Date')+
      scale_y_continuous(name='Relative temperature',limits = c(min(plotdata()),max(plotdata())))+
      ggtitle('Relative Temperature')+
      theme_bw()
    
  })
  
  output$loadplot <- renderPlot({
    
    
    ggplot(mapping=aes(x=dts.data$Load.time, y = dts.data$Load))+
      geom_line()+
      scale_x_datetime(
        limits = c(datelimit1(),datelimit2()),
        name = 'Time and Date')+
      scale_y_continuous(name='Load')+
      ggtitle('Load')+
      theme_bw()
    
  })
  
}

shinyApp(ui = ui, server = server)




