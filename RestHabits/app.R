
# https://mastering-shiny.org/action-layout.html
# https://shiny.rstudio.com/gallery/

# load packages
library(shiny)
library(ggplot2)
library(dplyr)
library(rsconnect) # ? needed

# read in data
rest_habits <- readRDS(here::here("RestHabits/shinydata.rds"))

# Define UI
ui <- fluidPage(
    # Application title
    titlePanel("Rest Habits at GVSU"),
    # Plot bar chart
    mainPanel(
        tabsetPanel(
            id = "tabset",
            tabPanel("Demographics",
                     fluidRow(
                         column(12, "Age")
                     ),
                     fluidRow(
                         column(12, "Sex")
                     ),
                     fluidRow(
                         column(12, "Race or Ethnicity")
                     ),
                     fluidRow(
                         column(12, "School Year")
                     ),
                     fluidRow(
                         column(12, "Type of Degree")
                     ), # probs make this is into a dropdown instead of making a longer page
                     ),
            
            tabPanel("Sleep",
                     fluidRow(
                         column(12, "Degree (or majors) vs Sleep Duration")
                     ),
                     fluidRow(
                         column(12, "Mental Health/Academics vs Sleep Duration")
                     ),
                     fluidRow(
                         column(12, "sleep enough vs Sleep Duration") # dropdown w/ sleep duration
                     ),
                     fluidRow(
                         column(12, "nap vs academics/gpa")
                     ),
                     fluidRow(
                         column(12, "nap vs energy (or others)")
                     ),
                     fluidRow(
                         column(12, "all nighters vs academics/gpa")
                     )
                     ),
            
            tabPanel("Leisure Time", 
                     fluidRow(
                         column(12, "majors vs leisure time")
                     ),
                     fluidRow(
                         column(12, "leisure time vs gpa")
                     ),
                     fluidRow(
                         column(12, "leisure time vs social media")
                     ),
                     fluidRow(
                         column(12, "soc media vs gpa")
                     ),
            ),
            
            tabPanel("Degree", 
                     fluidRow(
                         column(12, "oh probs move questions between majors here")
                     ),
                     fluidRow(
                         column(12, "hi")
                     ),
            )
            ),
        )
    )

# Define server
server <- function(input, output, session) {
    output$barChart <- renderPlot({
        ggplot(rest_habits, aes_string(x=input$x)) + 
            geom_bar() +
            coord_flip()
    })
    output$panel <- renderText({
        paste("Current panel: ", input$tabset)
    })
    output$gpa_hist <- renderPlot(
        ggplot(rest_habits, aes(x=gpa)) +
            geom_histogram()
    )
}

# Run app
shinyApp(ui = ui, server = server)

# sidebarPanel(
#     selectInput(
#         inputId = "x",
#         label = "Select question:",
#         choices = c(
#             "How would you describe your energy levels?" = "energy",
#             "How would you describe your stress levels?" = "stress",
#             "How would you describe your ability to concentrate?" = "concentration",
#             "How would you describe your overall mood?" = "mood",
#             "How do you feel you are doing academically?" = "academics"
#         ),
#         selected = "stress"
#     ),
# ),
# mainPanel(plotOutput("barChart"))






