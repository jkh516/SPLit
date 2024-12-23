# library
library(shiny)
library(bslib)
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
                                     "numeric", "numeric"), sheet = 1)

Data2223 <- read_excel("Data2324.xlsx", 
                       col_types = c("text", "numeric", "text", 
                                     "date", "date", "text", "text", "text", 
                                     "numeric", "text", "numeric", "numeric", 
                                     "numeric", "numeric"), sheet = 2)

Data1819 <- read_excel("Data2324.xlsx", 
                       col_types = c("text", "numeric", "text", 
                                     "date", "date", "text", "text", "text", 
                                     "numeric", "text", "numeric", "numeric", 
                                     "numeric", "numeric"), sheet = 3)

### Fix colours for consistent plotting
team.colours2324 <- c("Aberdeen" = "red", "Celtic"= "green","Dundee" = "black", "Hearts" = "purple", "Hibernian" = "deeppink", "Kilmarnock" = "grey", "Livingston" = "violet", "Motherwell" = "maroon", "Rangers" = "blue", "Ross County" ="deepskyblue", "St Johnstone" = "yellow", "St Mirren" ="cyan")
team.colours2223 <- c("Aberdeen" = "red", "Celtic"= "green","Dundee United" = "orange", "Hearts" = "purple", "Hibernian" = "deeppink", "Kilmarnock" = "grey", "Livingston" = "violet", "Motherwell" = "maroon", "Rangers" = "blue", "Ross County" ="deepskyblue", "St Johnstone" = "yellow", "St Mirren" ="cyan")
team.colours1819 <- c("Aberdeen" = "red", "Celtic"= "green","Dundee" = "black", "Hamilton" = "gold" , "Hearts" = "purple", "Hibernian" = "deeppink", "Kilmarnock" = "grey", "Livingston" = "violet", "Motherwell" = "maroon", "Rangers" = "blue", "St Johnstone" = "yellow", "St Mirren" ="cyan")


### Fix line styles for attendance proportion plot
custom_dashes2324 <- c("Aberdeen" = "Dash", "Celtic"= "Solid", "Dundee" = "Solid", "Hearts" = "Solid", "Hibernian" = "Dash", "Kilmarnock" = "Solid", "Livingston" = "Dash", "Motherwell" = "Dash", "Rangers" = "Solid", "Ross County" ="Dash", "St Johnstone" = "Dash", "St Mirren" ="Solid")
custom_dashes2223 <- c("Aberdeen" = "Solid", "Celtic"= "Solid", "Dundee United" = "Dash", "Hearts" = "Solid", "Hibernian" = "Solid", "Kilmarnock" = "Dash", "Livingston" = "Dash", "Motherwell" = "Dash", "Rangers" = "Solid", "Ross County" ="Dash", "St Johnstone" = "Dash", "St Mirren" ="Solid")
custom_dashes1819 <- c("Aberdeen" = "Solid", "Celtic"= "Solid", "Dundee" = "Dash", "hamilton" = "Dash", "Hearts" = "Solid", "Hibernian" = "Solid", "Kilmarnock" = "Solid", "Livingston" = "Dash", "Motherwell" = "Dash", "Rangers" = "Solid",  "St Johnstone" = "Dash", "St Mirren" ="Dash")


# Define UI for miles per gallon app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Exploring Home Attendance in the Scottish Premier League"), ################ Change this
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(width=2,
                 
                 selectInput("season", "Season:",c("2023/24",
                                                   "2022/23",
                                                   "2018/19"),
                             selected = "2023/24"),
                 
                 # Input: Selector for variable to plot against Group ----
                 selectInput("team", "Team:",
                             choices = NULL),
                 
                 uiOutput("image_table"),
                 
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(width=10, 
              
              ####
              # Output: Layout columns with cards w/ plot, summary, and table ----
              layout_columns(type = "tabs",
                          card(highchartOutput("AllPlot"),
                               # Output: Requested attendance plot ----
                               plotlyOutput("TeamPlot", height="auto", width="auto")
                               ),
                          
                          
                          ####
                          
                          
                          
              ),
    )
  ),
)

# Define server logic to plot various variables against Group ----
server <- function(input, output) {
  
  #image based on season
  
  output$image_table <- renderUI({
    if (input$season == "2023/24"){
      img(src="SPLTable2.png", height="100%", width="100%", align = "center")
    }else if(input$season == "2022/23"){
      img(src="SPLTable23.png", height="100%", width="100%", align = "center")
    }else if(input$season == "2018/19"){
      img(src="SPLTable19.png", height="100%", width="100%", align = "center")
    }
  })
  
  # select data based on season
  data <- reactive({
    if (input$season == "2023/24"){
      data = Data2324
    }else if(input$season == "2022/23"){
        data = Data2223
    }else if(input$season == "2018/19"){
      data = Data1819
    }
  })
  
  #colours based on season
  colours <- reactive({
    if (input$season == "2023/24"){
      colours = team.colours2324
    }else if(input$season == "2022/23"){
      colours = team.colours2223
    }else if(input$season == "2018/19"){
      colours = team.colours1819
    }
  })
  
  #dashes based on season
  dashes <- reactive({
    if (input$season == "2023/24"){
      dashes = custom_dashes2324
    }else if(input$season == "2022/23"){
      dashes = custom_dashes2223
    }else if(input$season == "2018/19"){
      dashes = custom_dashes1819
    }
  })
  
  #observe data and update team choices
  observeEvent(data(), {
    choices_teams <- sort(unique(data()$Home))
    updateSelectInput(inputId = "team", choices = choices_teams)
  })
  
  # Create team subset  
  team <- reactive({
    input$team
  })
  
  teamdata <- reactive({
    subset(data(), Home==team()) 
  })
  
  ymin <- reactive({min(teamdata()$Attendance)})
  ymax <- reactive({max(max(teamdata()$Attendance), teamdata()$Capacity[1])})
  
  p <- reactive({
    ggplot(teamdata() ,
           aes(x=MWk, y=Attendance)) +
      geom_rect(xmin = 33.5, xmax = 40, ymin = ymin(), ymax = ymax(), alpha = .1, fill = "blue", aes(text="Post Split")) +
      geom_line(data=teamdata(),aes(x=MWk,y=Attendance)) +
      geom_point(color='black', shape=21, size=2, aes(fill=factor(Away), text=paste("<b>Match Week:</b>",MWk,"<br><b>Date:</b>",Date, "<br><b>Attendance:</b>",Attendance, "<br><b>Opponent:</b>", Away))) +
      ggtitle(paste(team(), " Home Match Attendance (", teamdata()$Round[18], ")", sep="")) +
      xlab("Match Week") +
      ylim(ymin(), ymax()) +
      xlim(1, 39) + 
      geom_hline(yintercept=teamdata()$Capacity[1], linetype="dashed", color = "blue") +
      geom_vline(xintercept=33.5, color = "red") +
      labs(fill='Opponent')+
      scale_fill_manual(values=colours())
  })
  
  
  
  # Generate a plot of the requested team's attendance
  output$TeamPlot <- renderPlotly({
    #p()$x$data[[1]]$hoverinfo <- "none"
    ggplotly(p(),tooltip="text")
  })
  ########
  output$AllPlot <- renderHighchart({
    hc <- hchart(  data(), "line",   hcaes(x = MWk, y = Aprop, group = Home),  color = colours(), dashStyle=dashes() ) |>
      hc_title(text = "Attendance as a Proption of Stadium Capacity") |>
      hc_xAxis(title = list(text = "Match Week"), plotBands = list(list(color="lightgrey", from=33.5, to=39))) |>
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
