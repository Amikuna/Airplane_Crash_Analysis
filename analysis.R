library(tidyverse)
library(readr)
library(shiny)
library(shinydashboard)
library(plotly)
library(DT) #for making table fit on page

#at first importing this dataset had no problem but in some cases it throws some warning which as I understood doesn't effect my analysis
incidents <- read_csv("H:/Amiko/kiu/4/R/final project/database.csv")
incidents <- incidents[incidents$`Operator`!='MILITARY',]
View(incidents)

#preparing data for time tab
monthly <- incidents%>%
  group_by(timeframe=`Incident Month`)%>%
  summarise(incidents=n(),)
daily <- incidents%>%
  group_by(timeframe=`Incident Day`)%>%
  summarise(incidents=n())
yearly <- incidents%>%
  group_by(timeframe=`Incident Year`)%>%
  summarise(incidents=n())
View(yearly)  

#preparing data for phase tab
phase <- incidents%>%
  group_by(Phase=`Flight Phase`, Visibility=`Visibility`)%>%
  summarise(Amount=n(),)%>%
  arrange(-Amount)


for (i in c(1:length(phase$Visibility))){
  if (is.na(phase$Visibility[i])){
    phase$Visibility[i]='UNKNOWN'
  }
}

# I removed NA values because there were too many and almost all Phase NA had NA in Visibility column so 
# it didn't make sense
phase<-phase[!is.na(phase$Phase),]

View(phase)
#preparing data for visibility/species tab
loc <- incidents%>%
  group_by(State=`State`)%>%
  summarise(Amount=n())%>%
  arrange(-Amount)
# turned out all the NA states where outside us/canada
for (i in c(1:length(loc$State))){
  if (is.na(loc$State[i])){
    loc$State[i] <- 'Outside US/Canada'
  }
}


view(loc)

ui <- dashboardPage(
  dashboardHeader(title="Airplane incidents"),
  dashboardSidebar(uiOutput("tab1"), uiOutput("tab2")),#outputs for tab1 and tab2
  dashboardBody(
    fluidPage(
      fluidRow(
        column(
          width=12,
          tabBox(id="tabs", height="250px",
                 title="Airplane Accidents",
                 width=NULL,
                 tabPanel("Time", plotlyOutput(outputId = "plot1")),
                 tabPanel("Phase", plotlyOutput(outputId = "plot2")),
                 tabPanel("States", DT::DTOutput(outputId='table',height="500px"))#making table fit on page
                
          )
        )
      )
    )
  )
)
server <- function(input,output){
  dat<-reactive(
    if(input$select_time=='Month'){
      monthly
    }else if(input$select_time=='Day'){
      daily
    }else if(input$select_time=='Year'){
      yearly
    }
  )
  #function to put correct numbers in ranbow(len) it doesn't work otherwise
  len <- function(){
    if(input$select_time=='Month'){
      return(12)
    }else if(input$select_time=="Day"){
      return(31)
    }else if(input$select_time=="Year"){
      return(26)
    }
  }
  top_phase <- reactive(
    phase %>%
      head(input$select_topn)
  )
  output$plot1 <- renderPlotly({
    ggplotly(dat() %>%
      ggplot() + aes(x=reorder(timeframe,timeframe), y=incidents) +
      geom_bar(stat="identity", fill=rainbow(len()))+coord_flip()+
      labs(x='Time',y='Incidents'),tooltip = 'y')
  })
  
  output$plot2 <- renderPlotly({
    ggplotly(top_phase()%>%
      ggplot()+aes(x = reorder(x=Phase,Amount),y=Amount, fill=Visibility)+
      geom_bar(stat='identity',)+
      coord_flip()+labs(x='Flight Phase'),tooltip='y')
  })
  output$table <- renderDT({
    loc
  })
  
  #adding different inputs for different tabs
  output$tab1 <- renderUI({
    conditionalPanel(condition = 'input.tabs=="Time"',
                     selectInput(inputId ="select_time",
                                 label="Select Time Period",
                                 c('Year','Month','Day')))
  })
  output$tab2 <- renderUI({
    conditionalPanel(condition = 'input.tabs=="Phase"',
                     sliderInput(inputId ="select_topn",
                                 label="The Most Common Phases",
                                 value=20,min=3,max=64,step=1))
  })
  
}

shinyApp(ui=ui, server=server)


