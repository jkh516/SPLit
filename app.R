# library
library(shiny)
library(readxl)
library(ggplot2)
library(plotly)

library(ggiraph)
library(tidyverse)
library(highcharter) 

# Preamble ----

# Anything that doesn't rely on any user inputs,
# we can do this once at startup
# and then use the value throughout the lifetime of the app

### Load Data
Data2324 <- read_excel("Data2324.xlsx", 
                       col_types = c("text", "numeric", "text", 
                                     "date", "date", "text", "text", "text", 
                                     "numeric", "text", "numeric", "numeric", 
                                     "numeric", "numeric"))

### Fix colours for consistent plotting
team.colours2 <- c("Aberdeen" = "red", "Celtic"= "green", "Dundee" = "black", "Hearts" = "purple", "Hibernian" = "deeppink", "Kilmarnock" = "grey", "Livingston" = "violet", "Motherwell" = "orange", "Rangers" = "blue", "Ross County" ="deepskyblue", "St Johnstone" = "yellow", "St Mirren" ="cyan")

### Fix line styles for attendance proportion plot
custom_dashes <- c("Aberdeen" = "Dash", "Celtic"= "Solid", "Dundee" = "Solid", "Hearts" = "Solid", "Hibernian" = "Dash", "Kilmarnock" = "Solid", "Livingston" = "Dash", "Motherwell" = "Dash", "Rangers" = "Solid", "Ross County" ="Dash", "St Johnstone" = "Dash", "St Mirren" ="Solid")



# Define UI for miles per gallon app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Home Attendance 23/24"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(width=2,
      
      # Input: Selector for variable to plot against Group ----
      selectInput("team", "Team:",
                  c("Aberdeen" = "Aberdeen",
                    "Celtic" = "Celtic",
                    "Dundee" = "Dundee",
                    "Hearts" = "Hearts",
                    "Hibernian"="Hibernian",
                    "Kilmarnock" = "Kilmarnock",
                    "Livingston"  = "Livingston" ,
                    "Motherwell" = "Motherwell" ,
                    "Rangers" = "Rangers",
                    "Ross County" = "Ross County" ,
                    "St Johnstone" = "St Johnstone",
                    "St Mirren" = "St Mirren")),
      
      img(src="SPLTable2.png", height="100%", width="100%", align = "center"),
        
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(width=10, 
              
              ####
              # Output: Tabset w/ plot, summary, and table ----
              tabsetPanel(type = "tabs",
                          tabPanel("All Attendances", highchartOutput("AllPlot")),
                          # Output: Reqested attendance plot ----
                          tabPanel("Team Attendance", plotlyOutput("TeamPlot", height="auto", width="auto"))
              
              ####
      
     
      
    ),
  )
),
)

# Define server logic to plot various variables against Group ----
server <- function(input, output) {
  
  # Create team subset  
  team <- reactive({
    input$team
  })
  
  teamdata <- reactive({
    subset(Data2324, Home==team()) 
  })
  
  ymin <- reactive({min(teamdata()$Attendance)})
  ymax <- reactive({max(max(teamdata()$Attendance), teamdata()$Capacity[1])})
  
  p <- reactive({
    ggplot(teamdata() ,
           aes(x=MWk, y=Attendance)) +
      geom_rect(xmin = 33.5, xmax = 40, ymin = ymin(), ymax = ymax(), alpha = .1, fill = "blue", aes(text=NULL)) +
      geom_line(data=teamdata(),aes(x=MWk,y=Attendance)) +
      geom_point(color='black', shape=21, size=2, aes(fill=factor(Away), text=paste("<b>Match Week:</b>",MWk,"<br><b>Date:</b>",Date, "<br><b>Attendance:</b>",Attendance, "<br><b>Opponent:</b>", Away))) +
      ggtitle(paste(team(), " (", teamdata()$Venue[1], ")- ", teamdata()$Round[18], sep="")) +
      xlab("Match Week") +
      ylim(ymin(), ymax()) +
      xlim(1, 39) + 
      geom_hline(yintercept=teamdata()$Capacity[1], linetype="dashed", color = "blue") +
      geom_vline(xintercept=33.5, color = "red") +
      labs(fill='Opponent')+
      scale_fill_manual(values=team.colours2)
  })
  
  
  
  # Generate a plot of the requested team's attendance
  output$TeamPlot <- renderPlotly({
    #p()$x$data[[1]]$hoverinfo <- "none"
    ggplotly(p(),tooltip="text")
       })
########
  output$AllPlot <- renderHighchart({
    hc <- hchart(  Data2324, "line",   hcaes(x = MWk, y = Aprop, group = Home),  color = team.colours2, dashStyle=custom_dashes ) |>
      hc_title(text = "Attendance as a Proption of Stadium Capacity") |>
      hc_xAxis(title = list(text = "Match Week"), plotBands = list(list(color="lightgrey", from=34, to=39))) |>
      hc_yAxis(title = list(text = "Proportion"), max=1) |>
      hc_tooltip(crosshairs=TRUE, formatter = JS("function(){
                            return ('Team: ' + this.point.Home + '<br> Opponent: ' + this.point.Away + ' <br> Attendance: ' + this.point.Attendance + ' <br> Match Week: ' + this.point.MWk + '<br> Round: ' + this.point.Round)
                            }"))
    hc
  })
  
  ##########


}

# Create Shiny app ----
shinyApp(ui, server)
