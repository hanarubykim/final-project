library(shiny)
library(tidyverse)
library(shinythemes)
library(ggplot2)

source(file = "shiny_app/data.R")

# Define UI for application that draws a histogram
ui <- navbarPage(theme = shinytheme("flatly"),
                 "How NYC Students of Different Racial Groups Perform Academically Over Time",
                 tabPanel("Comparing Performance by Test",
                          fluidPage(
                              titlePanel("Introduction"),
                              h3("Understanding the New York City School System"),
                              p("The City School District of the City of New York is the largest school system in the United States, comprised of over 1.1 million students taught in more than 1,800 separate schools. Beginning from the third grade up until the 8th, students in NYC take two yearly benchmark exams: the English Language Arts (ELA) exam and the Math exam. The NYC Department of Education (DOE) uses these benchmark exams to assess whether students are deemed to be performing \"proficiently\" academically."),
                              p("Based on each student within a district, the DOE aggregates data on the overall proficiency rate for various subgroups, whether income or race. In my analysis, I will be focusing on race and age as a factor of academic proficiency."),
                              h3("\nComparing Performance by Test"),
                              p("Here, we can see the overall academic proficiency by racial subgroup for the 32 school districts of New York City based on the 2018 benchmark exams."),
                              sidebarLayout(
                                  sidebarPanel(
                                      selectInput(
                                          inputId = "test",
                                          label = "Choose a test",
                                          choices = c("English Language Arts" = "ela", 
                                            "Math" = "math"))),
                                      mainPanel(plotOutput("test_plot"))))
                 ),
                 tabPanel("Analysis",
                          titlePanel("Predicting Academic Performance Over Time For Each Racial Group"),
                          h3("Looking at Overall Proficiency By Each Racial Group"),
                          splitLayout(cellWidths = c("60%", "40%"),
                                      cellArgs = list(style = "padding: 3px"),
                                      imageOutput("overall",
                                                  width = 650,
                                                  height = 450),
                          gt_output("table")),
                          p("As shown above, there is a large disparity predicted between students of different racial groups in terms of the percentage of each racial group being able to meet academic proficiency standards for both math and ELA tests."),
                          h3("Looking at Proficiency by Grade"),
                          p("We can look at our data more closely by focusing on the performance of students of different racial groups over time."),
                          p("This model predicts how students of specific racial groups will perform on their benchmark tests over time by grade."),
                          imageOutput("grade",
                                      width = 700,
                                      height = 500),
                          p("\nEvidently, proficiency across all racial subgroups fall as they ascend to higher grades."),
                 ),
                 
                 tabPanel("About", 
                          titlePanel("About"),
                          h3("Project Background and Motivations"),
                          p("This issue is significant to me in particular as an alumni of the NYC public schooling system, but Stuyvesant High School in particular, one of the eight specialized high schools in NYC. For a specialized high school, admission is granted only through a standardized test called the Specialized High School Aptitude Test (SHSAT) which is normally taken in the fall by 8th graders. My former high school requires the highest cutoff score for SHSAT to be admitted, and such admissions procedure has turned out disproportionately low numbers in admission of Black and Hispanic students. I wanted to see if the benchmark exams required of every NYC student for six years could offer some insight onto the current state of the NYC school system today."),
                          h3("Data Sources"),
                          p("My data comes from the New York State Education Department, and I am using 32 district reports to make a comprehensive report on all of the districts within NYC."),
                          h3("About Me"),
                          p("Hi, I'm Hana Kim, a freshman at Harvard College who enjoys data science! You can reach me at hana_kim@college.harvard.edu."),
                          uiOutput("link"),
                 )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$link <- renderUI({
        tags$a(href="https://github.com/hanarubykim/final-project.git", "Here is the link to this repository.")
    })
    
    output$test_plot <- renderPlot({
        if(input$test == "ela"){
            ela_test_plot }
        else if(input$test == "math"){
            math_test_plot }
    })
    
    output$test_plot2 <- renderPlot({
               ela_test_plot
    })
    
    output$overall <- renderImage({
        list(
            src = "shiny_app/racial_group_pred.png",
            width = 650,
            height = 450)
    })
    
    output$table <- render_gt({
        table_1
    })
    
    output$grade <- renderImage({
        list(
            src = "shiny_app/grade_prediction.png",
            width = 700,
            height = 500)
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
