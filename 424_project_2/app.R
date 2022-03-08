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

#extra year and day of the week as new columns for each entry using lubridate functions
rider_data$datestamp <- as.Date(rider_data$date, "%m/%d/%Y") 
rider_data$year <- year(rider_data$datestamp)
rider_data$month <- month(rider_data$datestamp, label=TRUE)
rider_data$dayOfWeek <-wday(rider_data$datestamp, label=TRUE)

#merge both data sets by stationname
cta_data = merge(x=rider_data,y=l_stop_data,by="stationname")

# create menu items 
years <- c(2001:2021)
station_choices <- c("UIC-Halsted", "O'Hare Airport", "Washington/Dearborn")
graph_choices <- c("Total Entries per Year", "Total Entries per Date", "Total Entries per Month", "Total Entries per Day of Week")

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
      tabPanel("Single View",
             column(2, 
                    fluidRow(
                      box(title = "Control Panel", width = 12, height = 500,
                          selectInput("Year", "Select the year to visualize", years, selected = 2021),
                          selectInput("StationName", "Select the station you want to visualize", station_choices, selected = "UIC-Halsted")
                      )
                    )
             ),    
             column(8,
                    tabsetPanel(
                      tabPanel("Total Entries per Year", plotOutput("bar1"), height="500px"),
                      tabPanel("Total Entries per Month", plotOutput("bar2"), height="500px"),
                      tabPanel("Total Entries per Date", plotOutput("bar3"), height="500px"),
                      tabPanel("Total Entries per Day of Week", plotOutput("bar4"), height="500px"),
                      tabPanel("Table View", DT::dataTableOutput("tab1"), height="500px")
                    )
             ),
             ),
    tabPanel("Split View",
             column(6,
                    fluidRow(
                      tabsetPanel(
                        tabPanel("Total Entries per Year", plotOutput("bar5"), height="500px"),
                        tabPanel("Total Entries per Month", plotOutput("bar6"), height="500px"),
                        tabPanel("Total Entries per Date", plotOutput("bar7"), height="500px"),
                        tabPanel("Total Entries per Day of Week", plotOutput("bar8"), height="500px"),
                        tabPanel("Table View", DT::dataTableOutput("tab2"))
                      )
                    ),
                    fluidRow(
                      box(title = "Control Panel 1", width = 12, height = 500,
                          selectInput("Year2", "Select the year to visualize", years, selected = 2021),
                          selectInput("StationName2", "Select the station you want to visualize", station_choices, selected = "UIC-Halsted")
                      )
                    )
             ),
             column(6,
                    fluidRow(
                      tabsetPanel(
                        tabPanel("Total Entries per Year", plotOutput("bar9"), height="500px"),
                        tabPanel("Total Entries per Month", plotOutput("bar10"), height="500px"),
                        tabPanel("Total Entries per Date", plotOutput("bar11"), height="500px"),
                        tabPanel("Total Entries per Day of Week", plotOutput("bar12"), height="500px"),
                        tabPanel("Table View", DT::dataTableOutput("tab3"))
                      ),
                    ),
                    fluidRow(
                      box(title = "Control Panel 2", width = 12, height = 500,
                          selectInput("Year3", "Select the year to visualize", years, selected = 2021),
                          selectInput("StationName3", "Select the station you want to visualize", station_choices, selected = "O'Hare Airport")
                      )
                    )
             )
             
             ),
    tabPanel("About",
          mainPanel(
              h1("About Page"),
              p("The", tags$a(href="https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f", "CTA Ridership Data"),
                "is from the Chicago Data Portal. This dashboard was created by Megan Mehta and is the first project for CS 424 at UIC. This project
                was created in February 2022.")
          )
    ),
  )))
  
server <- function(input, output) {
  # increase the default font size
  theme_set(theme_grey(base_size = 14)) 
  
  # your dashboard should initially show a bar chart showing total entries for all of the 
  #L stations in alphabetical order for August 23, 2021 
  output$bar1 <- renderPlot({
    
    #figure out station name + get yearly entry counts 
    selected_date <- subset(cta_data, date=="08/23/2021")
    
    totalRidesperStation = aggregate(selected_date$rides, by = list(selected_date$stationname), FUN = sum)
    
    #TODO: fix spacing between bars, or make names more clear 
    ggplot(totalRidesperStation, aes(x=Group.1, y=x)) + geom_bar(stat="identity", fill="pink") + labs(x="Station Name", y = "Count") 
  })
  
  # a bar chart showing total entries at UIC-Halsted for each month for 2021 (jan, feb, ... dec)
  output$bar2 <- renderPlot({
    
    #get selected station value + display rides per day 
    selected_year <- subset(cta_data, year == input$Year)
    selected_station <- subset(selected_year, stationname == input$StationName)
    monthly_rides = aggregate(selected_station$rides, by = list(selected_station$month), FUN = sum)
    
    ggplot(monthly_rides, aes(x=Group.1, y=x)) + geom_bar(stat="identity", fill="steelblue") + labs(x="Month", y = "# of Rides") 
  })
  
  # a bar chart showing entries at UIC-Halsted each day for 2021 (jan 1, jan 2, ... dec 31)
  output$bar3 <- renderPlot({
    
    #get selected station value + display rides per day 
    selected_year <- subset(cta_data, year == input$Year)
    selected_station <- subset(selected_year, stationname == input$StationName)

    ggplot(selected_station, aes(x=date, y=rides)) + geom_bar(stat="identity", fill="steelblue") + labs(x="Date", y = "# of Rides") 
  })
  
  output$bar4 <- renderPlot({
    
    #get selected station value + display rides aggregate by day of week 
    selected_year <- subset(cta_data, year == input$Year)
    selected_station <- subset(selected_year, stationname == input$StationName)
    ridesByDayofWeek = aggregate(selected_station$rides, by = list(selected_station$dayOfWeek), FUN = sum)
    
    ggplot(ridesByDayofWeek, aes(x=Group.1, y=x)) + geom_bar(stat="identity", fill="steelblue") + labs(x="Day of Week", y = "# of Rides") 
  })
  
  #output table (just choose what dataframe to output and it will do it)
  output$tab1 <- DT::renderDataTable({
    selected_station <- subset(cta_data, stationname == input$StationName)
    selected_year <- subset(selected_station, year == input$Year)
    selected_year
  })
  
  #DUAL VIEW PART 1
  
  output$bar5 <- renderPlot({
    
    #figure out station name + get yearly entry counts 
    selected_station <- subset(cta_data, stationname == input$StationName2)
    countByYear = aggregate(selected_station$rides, by = list(selected_station$year), FUN = sum)
    
    ggplot(countByYear, aes(x=Group.1, y=x)) + geom_bar(stat="identity", fill="steelblue") + labs(x="Year", y = "Count") 
  })
  
  # a bar chart showing total entries at UIC-Halsted for each month for 2021 (jan, feb, ... dec)
  output$bar6 <- renderPlot({
    
    #get selected station value + display rides per day 
    selected_year <- subset(cta_data, year == input$Year2)
    selected_station <- subset(selected_year, stationname == input$StationName2)
    monthly_rides = aggregate(selected_station$rides, by = list(selected_station$month), FUN = sum)
    
    ggplot(monthly_rides, aes(x=Group.1, y=x)) + geom_bar(stat="identity", fill="steelblue") + labs(x="Month", y = "# of Rides") 
  })
  
  # a bar chart showing entries at UIC-Halsted each day for 2021 (jan 1, jan 2, ... dec 31)
  output$bar7 <- renderPlot({
    
    #get selected station value + display rides per day 
    selected_year <- subset(cta_data, year == input$Year2)
    selected_station <- subset(selected_year, stationname == input$StationName2)
    
    ggplot(selected_station, aes(x=date, y=rides)) + geom_bar(stat="identity", fill="steelblue") + labs(x="Date", y = "# of Rides") 
  })
  
  output$bar8 <- renderPlot({
    
    #get selected station value + display rides aggregate by day of week 
    selected_year <- subset(cta_data, year == input$Year2)
    selected_station <- subset(selected_year, stationname == input$StationName2)
    ridesByDayofWeek = aggregate(selected_station$rides, by = list(selected_station$dayOfWeek), FUN = sum)
    
    ggplot(ridesByDayofWeek, aes(x=Group.1, y=x)) + geom_bar(stat="identity", fill="steelblue") + labs(x="Day of Week", y = "# of Rides") 
  })
  
  #output table (just choose what dataframe to output and it will do it)
  #TODO: find way to link with check box 
  output$tab2 <- DT::renderDataTable({
    selected_station <- subset(cta_data, stationname == input$StationName2)
    selected_year <- subset(selected_station, year == input$Year2)
    selected_year
  })
  
  #DUAL VIEW PART 2
  
  output$bar9 <- renderPlot({
    
    #figure out station name + get yearly entry counts 
    selected_station <- subset(cta_data, stationname == input$StationName3)
    countByYear = aggregate(selected_station$rides, by = list(selected_station$year), FUN = sum)
    
    ggplot(countByYear, aes(x=Group.1, y=x)) + geom_bar(stat="identity", fill="steelblue") + labs(x="Year", y = "Count") 
  })
  
  # a bar chart showing total entries at UIC-Halsted for each month for 2021 (jan, feb, ... dec)
  output$bar10 <- renderPlot({
    
    #get selected station value + display rides per day 
    selected_year <- subset(cta_data, year == input$Year3)
    selected_station <- subset(selected_year, stationname == input$StationName3)
    monthly_rides = aggregate(selected_station$rides, by = list(selected_station$month), FUN = sum)
    
    ggplot(monthly_rides, aes(x=Group.1, y=x)) + geom_bar(stat="identity", fill="steelblue") + labs(x="Month", y = "# of Rides") 
  })
  
  # a bar chart showing entries at UIC-Halsted each day for 2021 (jan 1, jan 2, ... dec 31)
  output$bar11 <- renderPlot({
    
    #get selected station value + display rides per day 
    selected_year <- subset(cta_data, year == input$Year3)
    selected_station <- subset(selected_year, stationname == input$StationName3)
    
    ggplot(selected_station, aes(x=date, y=rides)) + geom_bar(stat="identity", fill="steelblue") + labs(x="Date", y = "# of Rides") 
  })
  
  output$bar12 <- renderPlot({
    
    #get selected station value + display rides aggregate by day of week 
    selected_year <- subset(cta_data, year == input$Year3)
    selected_station <- subset(selected_year, stationname == input$StationName3)
    ridesByDayofWeek = aggregate(selected_station$rides, by = list(selected_station$dayOfWeek), FUN = sum)
    
    ggplot(ridesByDayofWeek, aes(x=Group.1, y=x)) + geom_bar(stat="identity", fill="steelblue") + labs(x="Day of Week", y = "# of Rides") 
  })
  
  #output table (just choose what dataframe to output and it will do it)
  #TODO: find way to link with check box 
  output$tab3 <- DT::renderDataTable({
    selected_station <- subset(cta_data, stationname == input$StationName3)
    selected_year <- subset(selected_station, year == input$Year3)
    selected_year
  })
  
  
}

shinyApp(ui = ui, server = server,options = list(height = 1080))
