#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library(shiny)

# Define UI for application that draws a histogram
ui <- dashboardPage(
    
    # Application title
    dashboardHeader(title ="Boston Crime"),
    # Sidebar with a slider input for number of bins 
    
    dashboardSidebar(
        
        
        selectInput("Month", h3("Select Month"),
                    choices=unique(Crime_join$month), 
                    selected=10, 11, 12),
        
        checkboxGroupInput("Year", 
                           h3("Select Year"),
                           choices = unique(Crime_join$year), 
                           selected = "2017"),
        
        selectInput("Day", h3("Select Day"),
                    choices=unique(Crime_join$day_of_week), 
                    selected="Sunday"),
        
        checkboxGroupInput("Offense", 
                          h3("Select an Offense"), 
                           choices = unique(Crime_join$offense_code_group), 
                           selected = "Murder")
        
        
    ),
    
    ### Create the Dashboard Body 
    dashboardBody( 
        ### Create 3 rows to display the Output 
        fluidRow(
            valueBoxOutput("NumberofIncidents"),
            valueBoxOutput("ProportionofShooting"),
            valueBoxOutput("MedianShootingIncident"),
        ),
        fluidRow( 
            box( 
                status = "info", solidHeader = TRUE, 
                title = "Incidents Over Time", 
                plotOutput("IncidentsOverTime") ),
            
            box(
                status = "info", solidHeader = TRUE,
                title = "Location",
                leafletOutput("map")) ),
        
        fluidRow(
            box(
                status = "info", solidHeader = TRUE,
                title = "Words Used in Incident Description",
                plotOutput("WordCloud")),
            box(
                status = "info", solidHeader = TRUE,
                title = "Top Crime Streets",
                plotOutput("TopStreets"))
        )
        
        
        
    )
    
)


server <- function(input, output) {
    #1  
    #Number of Incidents
    output$NumberofIncidents<- renderValueBox({
        
       Crime_join%>%
            filter(month==input$Month, year==input$Year, day_of_week==input$Day, str_detect(offense_code_group, input$Offense))%>%
            distinct(incident_number, .keep_all = TRUE)%>%
            summarise(NumberofIncidents = n()) %>%
            prettyNum(big.mark = ",")%>%
            valueBox(subtitle = "Number of Incidents")
    })
    
    #Total Number of Reviews
    output$ProportionofShooting<- renderValueBox ({
        
        Crime_join%>%
            filter(month==input$Month, year==input$Year, day_of_week==input$Day, str_detect(offense_code_group, input$Offense))%>%
            distinct(incident_number, .keep_all = TRUE)%>%
            summarise(Count=n())%>%
            prettyNum(big.mark = ",")%>%
            valueBox(subtitle = "Number of Incidents")
            
    })
    
    #Average Incidents
            output$MedianShootingIncident<- renderValueBox({
                
                Crime_join%>%
                    filter(month==input$Month, year==input$Year, day_of_week==input$Day, str_detect(offense_code_group, input$Offense))%>%
                    distinct(incident_number, .keep_all = TRUE)%>%
                    #mutate(Number_of_words=str_count(text, boundary("word")))%>%
                    filter(shooting==1)%>%
                    summarise(Count=n())%>%
                    round()%>%
                    prettyNum(big.mark = ",")%>%
                    valueBox(subtitle = "Number of Shooting Incidents",color = "teal")
            })
    #2
    output$WordCloud <-renderPlot({
 
        Word_Cloud = function(x)
        {
            Crime_join %>%
                filter(month==input$Month, year==input$Year, day_of_week==input$Day, str_detect(offense_code_group, input$Offense))%>%
                unnest_tokens(word, offense_description) %>%
                filter(!word %in% stop_words$word) %>%
                filter(!word %in% c("mfr", "dist", "val","mv","person"))%>%
                count(word,sort = TRUE) %>%
                ungroup() %>%
                head(50) %>%
                with(wordcloud(word, n, max.words = 50,colors=brewer.pal(8, "Set1")))
        }
        
        Word_Cloud(offense_description)
    })
    
    output$IncidentsOverTime<-renderPlot({

            Crime_join%>%
            filter(month==input$Month, year==input$Year, day_of_week==input$Day, str_detect(offense_code_group, input$Offense))%>%
            #distinct(incident_number, .keep_all = TRUE)%>%
            select(year, incident_number)%>%
            group_by(year)%>%
            summarise(NumberofIncident = n())%>%
            ggplot()+
            geom_line(aes(x=year, y=NumberofIncident))+
            xlab("Year") + ylab("Number of Incidents")
        
    })
    
    
    #3
    output$map <-renderLeaflet({
        pal <- colorFactor(c("red","gray","orange","white","blue"),
                           domain = unique(Crime_join$offense_code))
        
        Crime_join_map<- Crime_join%>%
            filter(month==input$Month, year==input$Year, day_of_week==input$Day, str_detect(offense_code_group, input$Offense))%>%
            distinct(street, .keep_all = TRUE)
        
        map <- leaflet( Crime_join_map) %>%
            addProviderTiles("CartoDB.Positron") %>%
            addCircleMarkers(
                color = ~pal( Crime_join_map$offense_code),
                stroke = FALSE, fillOpacity = 0.5,
                lat =   Crime_join_map$lat,
                lng =  Crime_join_map$long,
                clusterOptions = markerClusterOptions(),
                popup = as.character( Crime_join_map$street))
        map
        
    })
    
    output$TopStreets <-renderPlot({
    
    Crime_join%>%
            filter(month==input$Month, year==input$Year, day_of_week==input$Day, str_detect(offense_code_group, input$Offense))%>%
        select(street, incident_number)%>%
        group_by(street)%>%
        filter(!is.na(street))%>%
        summarize(NumberofIncident = n())%>%
        arrange(desc(NumberofIncident))%>%
        head(10)%>%
        ggplot()+
        geom_point(aes(x = NumberofIncident, y=street))+
        geom_label(aes(x = NumberofIncident, y=street, label=street))+
        ggtitle("Top 10 Streets by Reported Incidents")+
        xlab("Number of Incidents") + ylab("Streets")
    })
}

shinyApp(ui, server)