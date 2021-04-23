library(shiny)
library(tidyverse)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("united"),
    "How NYC Students of Different Racial Groups Perform Academically Over Time",
    tabPanel("Comparing Performance by Test",
             mainPanel(titlePanel("Graphs of Data"),
                       imageOutput("data"),
                       imageOutput("data_2"))
    ),
    tabsetPanel(
        tabPanel("Comparing Performance by Test",
                 fluidPage(
                     titlePanel("NYSED 2018 Data"),
                     sidebarLayout(
                         sidebarPanel(
                             selectInput(
                                 "test",
                                 "Choose a 2018 Test",
                                 choices = c("English Language Arts" = "ela", 
                                             "Math" = "math")
                             ),
                             width = 300),
                         
                         plotOutput("test_plot",
                                    width = 550,
                                    height = 500)))),
        
        tabPanel("Predictions", 
                 titlePanel("Predicting Performance Over Time"),
                 imageOutput("grade"),
                 p("This model predicts how students of specific racial groups will perform on their benchmark tests over time by grade."),
                 h("Regression Table and Interpretation"),
                 gt_output("table1")),
        
        tabPanel("About", 
                 titlePanel("About"),
                 p("URL: https://github.com/hanarubykim/milestone-4.git"),
                 h3("Data Sources"),
                 p("Source: NYC DOE Demographic Snapshot (2013-2018)"),
                 p("This annual school account of NYC public school student populations record the breakdown of students, by district,
             race, and an economic need index."),
                 p("Source 2: NYC OpenData: Average SAT Scores for NYC Public Schools"),
                 p("This source details the average SAT scores by public high school."),
                 h3("Using the Sources"),
                 p("I will clean and combine the datasets so that I can track correlations between school district race makeup,
             school district income makeup, and the average school performance to see if I can identify correlations.")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$test_plot <- renderPlot({
        
        ifelse(input$test == "ela",
               list(
                   src = "ela.png",
                   width = 500,
                   height = 500),
               list(
                   src = "math.png",
                   width = 500,
                   height = 500))
    })
    
    output$grade <- renderPlot({
        list(
            src = "grade_prediction.png",
            width = 500,
            height = 500)
    })
    
    output$table1 <- render_gt({
        table
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
