
 library(shiny)
 library(ggmap)
 library(lubridate)

 crime <- read.csv("crime_data.csv")
 crime$date <- as.Date(parse_date_time(crime$date, "mdy"))
 crime1 <- subset(crime, offense == 'theft')
 crime <- crime[!(crime$offense %in% crime1$offense),]
 bbox <- c(left=-95.8, bottom=29.4, right=-95.0, top=30.0)
 map <- get_stamenmap(bbox, zoom = 10, source="stamen")
 # Define UI for application that draws a histogram
 ui <- fluidPage(

     # Application title
     titlePanel("Crime Plots for houston area"),

     # Sidebar with a slider input for number of bins
     sidebarLayout(
         sidebarPanel(
             checkboxGroupInput(
                 "offense", "offenses", choices = unique(crime$offense)
             ),
             sliderInput("date", "Range:",
                         min = as.Date(parse_date_time("01/01/2010", "dmy")), max = as.Date(parse_date_time("31/08/2010","dmy")), value = c(as.Date(parse_date_time("01/03/2010","dmy")),as.Date(parse_date_time("30/04/2010","dmy"))))
         ),

         # Show a plot of the generated distribution
         mainPanel(
            plotOutput("densityPlot"),
             plotOutput("datecrimePlot")
         )
     )
 )

 # Define server logic required to draw a histogram
 server <- function(input, output) {

     output$densityPlot <- renderPlot(

         ggmap(map) + stat_density2d(data = subset(crime, offense == input$offense),
                                     aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), geom = 'polygon') +
             scale_fill_gradient(low = "green", high = "red") +
             scale_alpha_continuous(range = c(0, 0.8)) +
             guides(fill = FALSE, alpha = FALSE) +
             ggtitle("Density plot of selected crimes") +xlab("Lon") + ylab("Lat")
     )
     output$datecrimePlot <- renderPlot(
         ggmap(map) + stat_density2d(data = subset(crime, date == input$date),
                                     aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), geom = 'polygon') +
             scale_fill_gradient(low = "green", high = "red") +
             scale_alpha_continuous(range = c(0, 0.8)) +
             guides(fill = FALSE, alpha = FALSE) +
             ggtitle("Spatial density of all the crimes for selected time frame.") +xlab("Lon") + ylab("Lat")
     )
 }

 # Run the application
 shinyApp(ui = ui, server = server)
