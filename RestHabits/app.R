
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
                         column(12, plotOutput("gpa_hist"))
                     ),
                     fluidRow(
                         column(12, "hi")
                     ),
                     ),
            tabPanel("Mental Health and Academics",
                     fluidRow(
                         sidebarPanel(
                         selectInput(
                             inputId = "x",
                             label = "Select question:",
                             choices = c(
                                 "How would you describe your energy levels?" = "energy",
                                 "How would you describe your stress levels?" = "stress",
                                 "How would you describe your abiility to concentrate?" = "concentration",
                                 "How would you describe your overall mood?" = "mood",
                                 "How do you feel you are doing academically?" = "academics"
                                 ),
                             selected = "stress"
                             ),
                         ),
                         mainPanel(plotOutput("barChart"))
                     )
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








