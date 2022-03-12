#libraries to include
library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(lubridate)

#import all data files realted to ridership and combine into one data frame 
ridership_files = list.files(pattern="^rider")

allData1 <- lapply(ridership_files, read.delim)
rider_data <- do.call(rbind, allData1)

#import additional data (locations of all stations) and rename column to make 'join' easier
l_stop_data <- read.csv("cta_data_l.csv")
names(l_stop_data)[names(l_stop_data) == 'STATION_NAME'] <- 'stationname' 

#separate long/lat into separate columns
l_stop_data$Location <- gsub("[()]", "", l_stop_data$Location) #get rid of parentheses
no_comma <- strsplit(l_stop_data$Location, ",") #remove comma 
rid_list <- unlist(no_comma) #get rid of extra list 
lat <- rid_list[seq(1,length(rid_list),2)]
l_stop_data$lat <- as.numeric(lat)
long <- rid_list[-seq(1,length(rid_list),2)]
l_stop_data$long <- as.numeric(long)

#extra year and day of the week as new columns for each entry using lubridate functions
rider_data$datestamp <- as.Date(rider_data$date, "%m/%d/%Y") 
rider_data$year <- year(rider_data$datestamp)
rider_data$month <- month(rider_data$datestamp, label=TRUE)
rider_data$dayOfWeek <-wday(rider_data$datestamp, label=TRUE)

#merge both data sets by stationname
cta_data = merge(x=rider_data,y=l_stop_data,by="stationname", all=TRUE)

# create menu items 
map_style <- c(providers$Esri.NatGeoWorldMap, providers$Stamen.Toner, providers$CartoDB.Positron)
years <- c(2001:2021)
station_choices <- unique(cta_data$stationname)
graph_choices <- c("Total Entries per Year", "Total Entries per Date", "Total Entries per Month", "Total Entries per Day of Week")
order <- c("Min/Max", "Alphabetical")

# Create the shiny dashboard
ui <- fluidPage(
  tags$div(
    tags$br(),
    tags$br(),
    tags$br(),
    tags$br(),
    tags$br(),
    tags$br(),
    tags$br(),
    tags$br(),
    tags$br(),
    tags$br(),
    tags$br(),
    tags$br(),
    tags$br(),
    tags$br(),
  ),
  titlePanel("CS 424 Spring 2022 Project 2 - Don't Sleep on the Subway"),
  fluidRow(
    navlistPanel(widths = c(2,10),
      tabPanel("Main",
               column(6,
                      DT::dataTableOutput("tab1"), height="500px",
               ),   
             column(6, 
                    fluidRow(
                      tabsetPanel(
                        tabPanel("Total Entries per Station", plotOutput("bar11"), height="500px"),
                        tabPanel("Map", leafletOutput("map1"), height="500px")
                      ),
                      box(title = "Control Panel", width = 12, height = 500,
                          selectInput("MapStyle", "Select the basemap", map_style, selected = providers$Stamen.Toner),
                          selectInput("DataOrdering", "Select order of bar chart", order, selected = "Alphabetical"),
                          #dateInput("Date", "Select date", value="2021-08-23", min="2001-01-01", max="2021-11-30", format="mm/dd/yy"),
                          textInput("Date", "Select date", value = "08/23/2021")
                      )
                    )
             ),),
    tabPanel("Plots",
             column(6,
                    fluidRow(
                      tabsetPanel(
                        tabPanel("Total Entries per Year", plotOutput("bar1"), height="500px"),
                        tabPanel("Total Entries per Month", plotOutput("bar2"), height="500px"),
                        tabPanel("Total Entries per Date", plotOutput("bar3"), height="500px"),
                        tabPanel("Total Entries per Day of Week", plotOutput("bar4"), height="500px")
                      ),
                      box(width = 12, height = 500,
                          selectInput("StationName", "Select the station you want to view", station_choices, selected = "UIC-Halsted"),
                          selectInput("Year", "Select the year to visualize", years, selected = 2021),
                      ),
                    ),
             ),
             column(6,
                    fluidRow(
                      DT::dataTableOutput("tab2"),
                      box(width = 12, height = 500,
                          selectInput("TableView", "Select what you want to see in the table", graph_choices, selected = "Total Entries per Year"),
                      ),
                    ),
             )),
    tabPanel("Two View",
             column(6,
                    fluidRow(
                      tabsetPanel(
                        tabPanel("Total Entries per Station", plotOutput("bar12"), height="500px"),
                        tabPanel("Map", leafletOutput("map2"), height="500px"),
                        tabPanel("Table View", DT::dataTableOutput("tab3"), height="500px")
                      ),
                      box(title = "Control Panel 1", width = 12, height = 500,
                          textInput("Date1", "Select date", value = "08/23/2021")
                      ),
                    ),
             ),
             column(6,
                    fluidRow(
                      tabsetPanel(
                        tabPanel("Total Entries per Station", plotOutput("bar14"), height="500px"),
                        tabPanel("Map", leafletOutput("map3"), height="500px"),
                        tabPanel("Table View", DT::dataTableOutput("tab4"), height="500px")
                      ),
                      box(title = "Control Panel 2", width = 12, height = 500,
                          textInput("Date2", "Select date", value = "08/24/2021")
                      ),
                    ),
                   
             )),
    tabPanel("About",
          mainPanel(
              h1("About Page"),
              p("The", tags$a(href="https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f", "CTA Ridership Data"),
                "and the", tags$a(href="https://data.cityofchicago.org/Transportation/CTA-System-Information-List-of-L-Stops/8pix-ypme", "CTA L Data"),
                "are both from the Chicago Data Portal. This dashboard was created by Megan Mehta and is the second project for CS 424 at UIC. This project
                was created in March 2022.")
          )
    ),
  )))
  
server <- function(input, output) {
  # increase the default font size
  theme_set(theme_grey(base_size = 14)) 
  
  # your dashboard should initially show a bar chart showing total entries for all of the 
  # L stations in alphabetical order for August 23, 2021 
  output$bar11 <- renderPlot({
    
    #figure out station name + get yearly entry counts 
    #selected_date <- subset(cta_data, date == format(input$Date, "%m/%d/%yyyy"))
    selected_date <- subset(cta_data, date == input$Date)

    totalRidesperStation <- aggregate(selected_date$rides, by = list(selected_date$stationname), FUN = sum)
    names(totalRidesperStation)[names(totalRidesperStation) == 'Group.1'] <- 'stationname' 
    
    #filter based on user selection 
    if(input$DataOrdering == "Alphabetical"){
      totalRidesperStation <- totalRidesperStation[order(totalRidesperStation$stationname),]
      ggplot(totalRidesperStation, aes(x=stationname, y=x)) + geom_bar(stat="identity", fill="pink") + labs(x="Station Name", y = "Count") + scale_y_continuous(label=comma)  + geom_col(width = 1) + theme(axis.text.x=element_text(angle=90,hjust=1))
      
    }
    else if (input$DataOrdering == "Min/Max"){
      totalRidesperStation <- totalRidesperStation[order(totalRidesperStation$x),]
      ggplot(totalRidesperStation, aes(reorder(stationname, x), x)) + geom_bar(stat="identity", fill="pink") + labs(x="Station Name", y = "Count") + scale_y_continuous(label=comma) + theme(axis.text.x=element_text(angle=90,hjust=1)) + geom_col(width = 1) 
      
    }
    
  })
  
  # your dashboard should also initially show a pannable / zoomable / resettable leaflet map of Chicago at 
  # an appropriate scale with an appropriate background that clearly shows all of the L stations and their 
  #total entries for the date above in graphical form with an appropriate legend
  output$map1 <- renderLeaflet({
    
    #get totals for initial date 
    selected_date <- subset(cta_data, date == input$Date)
    totalRidesperStation <- aggregate(selected_date$rides, by = list(selected_date$stationname), FUN = sum)
    names(totalRidesperStation)[names(totalRidesperStation) == 'Group.1'] <- 'stationname' 
    totalWithCoord = merge(x=totalRidesperStation,y=l_stop_data,by="stationname")
    
    #create color legend 
    colorPal <- colorNumeric(
      palette = c("red", "orange", "yellow"),
      domain = totalWithCoord$x
    )
    
    #create map
    leaflet(totalWithCoord) %>%
    addProviderTiles(input$MapStyle) %>%
    addCircleMarkers(lng = ~totalWithCoord$long, 
                    lat = ~totalWithCoord$lat,
                    label = paste("Station Name =", totalWithCoord$stationname),
                    stroke = FALSE, fillOpacity = 0.5,
                    color = ~colorPal(totalWithCoord$x)) %>%
    addLegend("bottomright", pal=colorPal, 
                          values=totalWithCoord$x,
                          title = "Total Rides per Station",
                          opacity=1)
  })

  
  #output table (just choose what dataframe to output and it will do it)
  output$tab1 <- DT::renderDataTable({
    #figure out station name + get yearly entry counts 
    selected_date <- subset(cta_data, date == input$Date)
    totalRidesperStation <- aggregate(selected_date$rides, by = list(selected_date$stationname), FUN = sum)
    names(totalRidesperStation)[names(totalRidesperStation) == 'Group.1'] <- 'stationname' 
    names(totalRidesperStation)[names(totalRidesperStation) == 'x'] <- 'Total_Rides' 
    
    #filter based on user selection 
    if(input$DataOrdering == "Alphabetical"){
      totalRidesperStation <- totalRidesperStation[order(totalRidesperStation$stationname),]
    }
    else if (input$DataOrdering == "Min/Max"){
      totalRidesperStation <- totalRidesperStation[order(totalRidesperStation$Total_Rides),]
    }
    
  })
  
  
  #PAGE 2 - select a station by tapping on a station on the map and see that stop 
  #highlighted on the map and bring up the data shown in the 60% part of Project 1.
  
  output$bar1 <- renderPlot({
    
    #figure out station name + get yearly entry counts 
    selected_station <- subset(cta_data, stationname == input$StationName)
    countByYear = aggregate(selected_station$rides, by = list(selected_station$year), FUN = sum)
    names(countByYear)[names(countByYear) == 'Group.1'] <- 'stationname' 
    names(countByYear)[names(countByYear) == 'x'] <- 'Total_Rides'
    
    ggplot(countByYear, aes(x=stationname, y=Total_Rides)) + geom_bar(stat="identity", fill="steelblue") + labs(x="Year", y = "Count") + scale_y_continuous(label=comma) 
  })
  
  # a bar chart showing total entries at UIC-Halsted for each month for 2021 (jan, feb, ... dec)
  output$bar2 <- renderPlot({
    
    #get selected station value + display rides per day 
    selected_year <- subset(cta_data, year == input$Year)
    selected_station <- subset(selected_year, stationname == input$StationName)
    monthly_rides = aggregate(selected_station$rides, by = list(selected_station$month), FUN = sum)
    
    ggplot(monthly_rides, aes(x=Group.1, y=x)) + geom_bar(stat="identity", fill="steelblue") + labs(x="Month", y = "# of Rides") + scale_y_continuous(label=comma)
  })
  
  # a bar chart showing entries at UIC-Halsted each day for 2021 (jan 1, jan 2, ... dec 31)
  output$bar3 <- renderPlot({
    
    #get selected station value + display rides per day 
    selected_year <- subset(cta_data, year == input$Year)
    selected_station <- subset(selected_year, stationname == input$StationName)
    
    ggplot(selected_station, aes(x=date, y=rides)) + geom_bar(stat="identity", fill="steelblue") + labs(x="Date", y = "# of Rides") + scale_y_continuous(label=comma) 
  })
  
  output$bar4 <- renderPlot({
    
    #get selected station value + display rides aggregate by day of week 
    selected_year <- subset(cta_data, year == input$Year)
    selected_station <- subset(selected_year, stationname == input$StationName)
    ridesByDayofWeek = aggregate(selected_station$rides, by = list(selected_station$dayOfWeek), FUN = sum)
    
    ggplot(ridesByDayofWeek, aes(x=Group.1, y=x)) + geom_bar(stat="identity", fill="steelblue") + labs(x="Day of Week", y = "# of Rides") + scale_y_continuous(label=comma)  
  })
  
  #output table (just choose what dataframe to output and it will do it)
  output$tab2 <- DT::renderDataTable({

    #filter based on user selection 
    if(input$TableView == "Total Entries per Year"){
      selected_station <- subset(cta_data, stationname == input$StationName)
      countByYear = aggregate(selected_station$rides, by = list(selected_station$year), FUN = sum)
      countByYear
    }
    else if (input$TableView == "Total Entries per Month"){
      selected_year <- subset(cta_data, year == input$Year)
      selected_station <- subset(selected_year, stationname == input$StationName)
      monthly_rides = aggregate(selected_station$rides, by = list(selected_station$month), FUN = sum)
      monthly_rides
    }
    else if (input$TableView == "Total Entries per Date"){
      selected_year <- subset(cta_data, year == input$Year)
      selected_station <- subset(selected_year, stationname == input$StationName)
      selected_station
    }
    else if (input$TableView ==  "Total Entries per Day of Week"){
      selected_year <- subset(cta_data, year == input$Year)
      selected_station <- subset(selected_year, stationname == input$StationName)
      ridesByDayofWeek = aggregate(selected_station$rides, by = list(selected_station$dayOfWeek), FUN = sum)
      ridesByDayofWeek
    }
    
    
  })
  
  #PAGE 3 - allow the user to specify two dates from a menu and the map and bar charts should show the 
  #change in entries between those two days using a divergent color scheme. The table should also show 
  #the change between those two days for each station.
  # your dashboard should initially show a bar chart showing total entries for all of the 
  # L stations in alphabetical order for August 23, 2021 
  output$bar12 <- renderPlot({
    
    #figure out station name + get yearly entry counts 
    #selected_date <- subset(cta_data, date == format(input$Date, "%m/%d/%yyyy"))
    selected_date <- subset(cta_data, date == input$Date1)
    
    totalRidesperStation <- aggregate(selected_date$rides, by = list(selected_date$stationname), FUN = sum)
    names(totalRidesperStation)[names(totalRidesperStation) == 'Group.1'] <- 'stationname' 
    
    #filter based on user selection 
    if(input$DataOrdering == "Alphabetical"){
      totalRidesperStation <- totalRidesperStation[order(totalRidesperStation$stationname),]
      ggplot(totalRidesperStation, aes(x=stationname, y=x)) + geom_bar(stat="identity", fill="pink") + labs(x="Station Name", y = "Count") + scale_y_continuous(label=comma)  + geom_col(width = 1) + theme(axis.text.x=element_text(angle=90,hjust=1))
      
    }
    else if (input$DataOrdering == "Min/Max"){
      totalRidesperStation <- totalRidesperStation[order(totalRidesperStation$x),]
      ggplot(totalRidesperStation, aes(reorder(stationname, x), x)) + geom_bar(stat="identity", fill="pink") + labs(x="Station Name", y = "Count") + scale_y_continuous(label=comma) + theme(axis.text.x=element_text(angle=90,hjust=1)) + geom_col(width = 1) 
      
    }
    
  })
  
  # your dashboard should also initially show a pannable / zoomable / resettable leaflet map of Chicago at 
  # an appropriate scale with an appropriate background that clearly shows all of the L stations and their 
  #total entries for the date above in graphical form with an appropriate legend
  output$map2 <- renderLeaflet({
    
    #get totals for initial date 
    #selected_date <- cta_data[cta_data$date == format(input$Date, "%m/%d/%yyyy"),]
    selected_date <- subset(cta_data, date == input$Date1)
    totalRidesperStation <- aggregate(selected_date$rides, by = list(selected_date$stationname), FUN = sum)
    names(totalRidesperStation)[names(totalRidesperStation) == 'Group.1'] <- 'stationname' 
    totalWithCoord = merge(x=totalRidesperStation,y=l_stop_data,by="stationname")
    
    #create color legend 
    colorPal <- colorNumeric(
      palette = c("blue", "green", "yellow"),
      domain = totalWithCoord$x
    )
    
    #create map
    leaflet(totalWithCoord) %>%
      addProviderTiles(input$MapStyle) %>%
      addCircleMarkers(lng = ~totalWithCoord$long, 
                       lat = ~totalWithCoord$lat,
                       label = paste("Station Name =", totalWithCoord$stationname),
                       stroke = FALSE, fillOpacity = 0.5,
                       color = ~colorPal(totalWithCoord$x)) %>%
      addLegend("bottomright", pal=colorPal, 
                values=totalWithCoord$x,
                title = "Total Rides per Station",
                opacity=1)
  })
  
  
  #output table (just choose what dataframe to output and it will do it)
  output$tab3 <- DT::renderDataTable({
    #figure out station name + get yearly entry counts 
    selected_date <- subset(cta_data, date == input$Date1)
    totalRidesperStation <- aggregate(selected_date$rides, by = list(selected_date$stationname), FUN = sum)
    names(totalRidesperStation)[names(totalRidesperStation) == 'Group.1'] <- 'stationname' 
    names(totalRidesperStation)[names(totalRidesperStation) == 'x'] <- 'Total_Rides' 
    
    #filter based on user selection 
    if(input$DataOrdering == "Alphabetical"){
      totalRidesperStation <- totalRidesperStation[order(totalRidesperStation$stationname),]
    }
    else if (input$DataOrdering == "Min/Max"){
      totalRidesperStation <- totalRidesperStation[order(totalRidesperStation$Total_Rides),]
    }
    
  })
  
  output$bar14 <- renderPlot({
    
    #figure out station name + get yearly entry counts 
    #selected_date <- subset(cta_data, date == format(input$Date, "%m/%d/%yyyy"))
    selected_date <- subset(cta_data, date == input$Date2)
    
    totalRidesperStation <- aggregate(selected_date$rides, by = list(selected_date$stationname), FUN = sum)
    names(totalRidesperStation)[names(totalRidesperStation) == 'Group.1'] <- 'stationname' 
    
    #filter based on user selection 
    if(input$DataOrdering == "Alphabetical"){
      totalRidesperStation <- totalRidesperStation[order(totalRidesperStation$stationname),]
      ggplot(totalRidesperStation, aes(x=stationname, y=x)) + geom_bar(stat="identity", fill="pink") + labs(x="Station Name", y = "Count") + scale_y_continuous(label=comma)  + geom_col(width = 1) + theme(axis.text.x=element_text(angle=90,hjust=1))
      
    }
    else if (input$DataOrdering == "Min/Max"){
      totalRidesperStation <- totalRidesperStation[order(totalRidesperStation$x),]
      ggplot(totalRidesperStation, aes(reorder(stationname, x), x)) + geom_bar(stat="identity", fill="pink") + labs(x="Station Name", y = "Count") + scale_y_continuous(label=comma) + theme(axis.text.x=element_text(angle=90,hjust=1)) + geom_col(width = 1) 
      
    }
    
  })
  
  # your dashboard should also initially show a pannable / zoomable / resettable leaflet map of Chicago at 
  # an appropriate scale with an appropriate background that clearly shows all of the L stations and their 
  #total entries for the date above in graphical form with an appropriate legend
  output$map3 <- renderLeaflet({
    
    #get totals for initial date 
    #selected_date <- cta_data[cta_data$date == format(input$Date, "%m/%d/%yyyy"),]
    selected_date <- subset(cta_data, date == input$Date2)
    totalRidesperStation <- aggregate(selected_date$rides, by = list(selected_date$stationname), FUN = sum)
    names(totalRidesperStation)[names(totalRidesperStation) == 'Group.1'] <- 'stationname' 
    totalWithCoord = merge(x=totalRidesperStation,y=l_stop_data,by="stationname")
    
    #create color legend 
    colorPal <- colorNumeric(
      palette = c("red", "orange", "yellow"),
      domain = totalWithCoord$x
    )
    
    #create map
    leaflet(totalWithCoord) %>%
      addProviderTiles(input$MapStyle) %>%
      addCircleMarkers(lng = ~totalWithCoord$long, 
                       lat = ~totalWithCoord$lat,
                       label = paste("Station Name =", totalWithCoord$stationname),
                       stroke = FALSE, fillOpacity = 0.5,
                       color = ~colorPal(totalWithCoord$x)) %>%
      addLegend("bottomright", pal=colorPal, 
                values=totalWithCoord$x,
                title = "Total Rides per Station",
                opacity=1)
  })
  
  
  #output table (just choose what dataframe to output and it will do it)
  output$tab4 <- DT::renderDataTable({
    #figure out station name + get yearly entry counts 
    selected_date <- subset(cta_data, date == input$Date2)
    totalRidesperStation <- aggregate(selected_date$rides, by = list(selected_date$stationname), FUN = sum)
    names(totalRidesperStation)[names(totalRidesperStation) == 'Group.1'] <- 'stationname' 
    names(totalRidesperStation)[names(totalRidesperStation) == 'x'] <- 'Total_Rides' 
    
    #filter based on user selection 
    if(input$DataOrdering == "Alphabetical"){
      totalRidesperStation <- totalRidesperStation[order(totalRidesperStation$stationname),]
    }
    else if (input$DataOrdering == "Min/Max"){
      totalRidesperStation <- totalRidesperStation[order(totalRidesperStation$Total_Rides),]
    }
    
  })
  
}

shinyApp(ui = ui, server = server,options = list(height = 1080))
