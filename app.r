library(googledrive)
library(shiny)
library(plotly)
#library(rsconnect)

##drive_auth(reset = TRUE)
setwd("/var/www/R-Shiny/PubShiny/")
#load("C:/Users/jbingham/Documents/.httr-oauth")

drive_find(n_max = 50)
##pubs_f.csv GoogleDriveID 1PieIAIrdPtHENHI8sUWo-9aWMj-XZrQuqdoc5gZnUvM
# AIzaSyA5LYFgnzn9D6fiGNEHugdcTnXPeZub_7Y
file_id <- "1PieIAIrdPtHENHI8sUWo-9aWMj-XZrQuqdoc5gZnUvM"
ImportPath <- "/var/www/R-Shiny/PubsShiny/pubs_f"

tryCatch({ 
  PubsData <- drive_download(as_id(file_id) , path = ImportPath,type = "csv",overwrite = TRUE )
}, error=function(e){})

# overwrite = TRUE,verbose = TRUE
PubsData <- read.csv(paste0(ImportPath,".csv"), header = TRUE)
#setwd("C:/Users/jbingham/Documents/R/PubsShiny")
saveRDS(PubsData, ".pubs_f.RDS")
PubsData$Price.of.Guinness <- as.numeric(substring(PubsData$Price.of.Guinness,2,7)) 

PubsData$LowNames <- tolower(PubsData$Names)
PubsDataSearch <-PubsData
PricePubData <- PubsData[!is.na(PubsData$Price.of.Guinness),]
PricePubData$PriceRange <- PricePubData$Price.of.Guinness
PricePubData$PriceRange[PricePubData$Price.of.Guinness>5.5]="Price > 5.50"
PricePubData$PriceRange[PricePubData$Price.of.Guinness<=5.5]="5 < Price <= 5.50"
PricePubData$PriceRange[PricePubData$Price.of.Guinness<=5]="4.50 < Price <= 5"
PricePubData$PriceRange[PricePubData$Price.of.Guinness<=4.5]="Price <= 4.50"

PubVisited <- PubsData[!is.na(PubsData$LastVerifiedBy),]
PubNotVisited <- PubsData[is.na(PubsData$Price.of.Guinness),]
OrderPrice<- subset(PricePubData,select=c(Names,Price.of.Guinness,LastVerifiedBy,LastVerifiedOn))
CheapestPints <- head(OrderPrice[order(OrderPrice$Price.of.Guinness),],5)
DearestPints <- head(OrderPrice[order(-OrderPrice$Price.of.Guinness),],5)
AveragePrice <- paste0(format(round(mean(OrderPrice$Price.of.Guinness),2),nsmall = 2))
PercentageVisited <- paste0(round((NROW(PubsData)-NROW(PubNotVisited))*100/NROW(PubsData),2),'%')
closedPubs <- NROW(PubsData[PubsData$X=="C",])


## https://www.mapbox.com/
Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1IjoiamltbXliaW5naGFtIiwiYSI6ImNqN2J1c3FqNTByaXIzM204bHE0NnBxN3MifQ.GXjAScMJ76T7H1L4-8ZlYw')



ui <- fluidPage(
  titlePanel("Guindex Pub Analysis"),
  
  # Give the page a title
  
  
  # Generate a row with a sidebar
  
  
  # Create a spot for the scatter plot
  mainPanel(
    
    fluidRow( 
      column(width = 8,
             div(style = "height:500px;",
                 
                 tabsetPanel(
                   tabPanel("Price", 
                            plotlyOutput("PriceCat")
                            
                   ), 
                   tabPanel("Contribution", 
                            #fluidRow(
                            #  column(width = 2,
                            #         selectInput("JBID", "James Bingham", choices = c("1","NA")) ,
                            #         selectInput("RMGID", "Robbie Mc Guinness", choices = c("1","NA"))
                            #  ),
                            # column(width = 2,
                            #         selectInput("DPID", "Daniel Purdy", choices = c("1","NA")) ,
                            #         selectInput("COCID", "Colin O Callaghan", choices = c("1","NA"))
                            #  ),
                            #  column(width = 2,
                            #         selectInput("ESID", "Emmet Sheerin", choices = c("1","NA")) ,
                            #         selectInput("BOCID", "Bill O Cleirigh", choices = c("1","NA"))
                            #  ),
                            #  column(width = 2,
                            #         selectInput("KONID", "Katie O'Neill", choices = c("1","NA")) 
                            #  )
                            #)
                            #,
                            plotlyOutput("contrib")
                            
                            
                   ),
                   tabPanel("Find a Pub",
                            fluidRow(
                              column(width = 12,
                                     selectInput("NotVisited", "Filter on Not-Visited Pubs", choices = c("ALL","NotVisited")) 
                                     
                              ))
                            ,textInput("PubSearchID", "Search for a Pub",""),plotlyOutput("AllPubs")
                   ),
                   tabPanel("Stats",
                            fluidRow(
                              column(width = 12,
                                     h4("Average Price of a Pint: ", AveragePrice),
                                     h4("No. of Pubs in Database: ",NROW(PubsData)),
                                     h4("No. of Pubs Visited: ",NROW(PubsData)-NROW(PubNotVisited)),
                                     h4("% of Pubs Visited: ",PercentageVisited),
                                     h4("No. of Pubs Closed: ",closedPubs),
                                     
                                     plotOutput(
                                       "HistoPlot"
                                     ),
                                     sliderInput(inputId = "n_breaks",
                                                 label = "Number of bins in histogram (approximate):",
                                                 min = 10, max = 50, value = 20, step = 10)
                                     
                              )
                            )
                            
                   )
                   # fluidRow(
                   #   column(width = 4,
                   #          textInput('PubSearchID', 'Search for a Pub')
                   #   ),
                   #   column(width = 3, offset = 2,
                   #          plotlyOutput("AllPubs")
                   #   )
                   #  )
                   #)
                   
                 )
                 
             )
             
             
      )
      ,
      column(width = 4, h4("Top 5 Cheapest Pints"),
             tableOutput("Cheap"),
             h4("Top 5 Most Expensive Pints"),
             tableOutput("Dear")
      )
    )
    
  )
)



server <- function(input, output) {
  
  
  
  # renderPlotly() also understands ggplot2 objects!
  output$Cheap <- renderTable({
    CheapestPints
  })
  
  output$Dear <- renderTable({
    DearestPints
  })
  
  output$HistoPlot <- renderPlot({
    
    hist(PricePubData$Price.of.Guinness,
         probability = FALSE,
         breaks = as.numeric(input$n_breaks),
         xlab = "Price of Pint",
         main = "Histogram of Pint Prices",
         col="light blue")
    
  })
  
  
  output$PriceCat <- renderPlotly({
    
    ### Price Categories
    plot_mapbox(mode = 'scattermapbox', split = ~PriceRange) %>%
      add_markers(
        data = PricePubData, x = ~Longitudes, y = ~Latitudes, 
        text= ~paste(Names,":",Price.of.Guinness) , 
        #color= I("red"),
        #fillcolor = c('green','red'),
        
        size = I(10), 
        hoverinfo = "text", 
        alpha = 1) %>%
      layout( mapbox = list(zoom = 12,
                            center = list(lat = ~median(Latitudes),
                                          lon = ~median(Longitudes))))
    
  })
  
  output$contrib <- renderPlotly({
    
    JBPub <- input$JBID
    RMPub <- input$RMGID
    DPPub <- input$DPID
    COCPub <- input$COCID
    ESPub <- input$ESID
    BOCPub <- input$BOCID
    KONPub <- input$KONID
    
    PubVisited <- PricePubData[PricePubData$JB == JBPub | PricePubData$RMG == RMPub | PricePubData$DP == DPPub | PricePubData$COC == COCPub | PricePubData$BOC == BOCPub | PricePubData$ES == ESPub | PricePubData$KON == KONPub,]
    PubVisitedEX <- PricePubData[!is.na(PricePubData$Names),]
    
    plot_mapbox(mode = 'scattermapbox', split = ~paste("LastVerifiedBy: ",LastVerifiedBy) ) %>%
      add_markers(
        data = PubVisitedEX, x = ~Longitudes, y = ~Latitudes, 
        text= ~paste(Names,":",Price.of.Guinness,"<br>",LastVerifiedOn) , 
        size = I(10), 
        hoverinfo = "text", 
        alpha = 1) %>%
      layout( mapbox = list(zoom = 12,
                            center = list(lat = ~median(Latitudes),
                                          lon = ~median(Longitudes))))
    
  })
  
  output$AllPubs <- renderPlotly({
    
    searchText <- tolower(input$PubSearchID)
    vFilter <- input$NotVisited
    
    if (vFilter == 'NotVisited') {
      
      
      if (!is.na(searchText)) {
        
        PubsDataSearch <- PubNotVisited[regexpr(searchText , PubNotVisited$LowNames) > 0 ,]
        ### run the code below if you are looking for a pub in the DB
        plot_mapbox(mode = 'scattermapbox') %>%
          add_markers(
            data = PubsDataSearch, x = ~Longitudes, y = ~Latitudes, 
            text= ~paste(Names) , 
            color= I("red"),
            #fillcolor = c('green','red'),
            
            size = I(10), 
            hoverinfo = "text", 
            alpha = 0.75) %>%
          layout( mapbox = list(zoom = 12,
                                center = list(lat = ~median(Latitudes),
                                              lon = ~median(Longitudes))))
      }else{
        plot_mapbox(mode = 'scattermapbox') %>%
          add_markers(
            data = PubNotVisited, x = ~Longitudes, y = ~Latitudes, 
            text= ~paste(Names) , 
            color= I("red"),
            #fillcolor = c('green','red'),
            
            size = I(10), 
            hoverinfo = "text", 
            alpha = 0.75) %>%
          layout( mapbox = list(zoom = 12,
                                center = list(lat = ~median(Latitudes),
                                              lon = ~median(Longitudes))))
      }
      
      
    }else{
      
      
      
      if (!is.na(searchText)) {
        
        PubsDataSearch <- PubsData[regexpr(searchText , PubsData$LowNames) > 0 ,]
        ### run the code below if you are looking for a pub in the DB
        plot_mapbox(mode = 'scattermapbox') %>%
          add_markers(
            data = PubsDataSearch, x = ~Longitudes, y = ~Latitudes, 
            text= ~paste(Names) , 
            color= I("red"),
            #fillcolor = c('green','red'),
            
            size = I(10), 
            hoverinfo = "text", 
            alpha = 0.75) %>%
          layout( mapbox = list(zoom = 12,
                                center = list(lat = ~median(Latitudes),
                                              lon = ~median(Longitudes))))
      }else{
        plot_mapbox(mode = 'scattermapbox') %>%
          add_markers(
            data = PubsData, x = ~Longitudes, y = ~Latitudes, 
            text= ~paste(Names) , 
            color= I("red"),
            #fillcolor = c('green','red'),
            
            size = I(10), 
            hoverinfo = "text", 
            alpha = 0.75) %>%
          layout( mapbox = list(zoom = 12,
                                center = list(lat = ~median(Latitudes),
                                              lon = ~median(Longitudes))))
      }
    }
    
    
  })
  
}

app <- shinyApp(ui, server)
options(shiny.port = 7724)
#options(shiny.port = 139)


runApp(app,port = getOption("shiny.port"), host = getOption("shiny.host", "45.79.148.4"),launch.browser=FALSE)
#runApp(app,port = getOption("shiny.port"), host = getOption("shiny.host", "192.168.10.30"),launch.browser=TRUE)
