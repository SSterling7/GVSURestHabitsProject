
# https://mastering-shiny.org/action-layout.html
# https://shiny.rstudio.com/gallery/

# load packages
library(shiny)
library(ggplot2)
library(dplyr)

# read in data
rest_habits <- readRDS(here::here("RestHabits/shinydata.rds"))

# Define UI
ui <- fluidPage(
    # Application title
    titlePanel("Rest Habits at GVSU"),

    # Sidebar with options for axis of bar chart graph (ranking selves on mental health)
    sidebarLayout(
        sidebarPanel(
           selectInput(
               inputId = "x",
               label = "Questions about mental health:",
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
         
        # Plot bar chart
        mainPanel(
           plotOutput("barChart")
        )
    )
)

# Define server
server <- function(input, output, session) {
    output$barChart <- renderPlot({
        ggplot(rest_habits, aes_string(x=input$x)) + 
            geom_bar() +
            coord_flip()
    })
    
}

# Run app
shinyApp(ui = ui, server = server)








