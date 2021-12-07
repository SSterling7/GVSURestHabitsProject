
# https://mastering-shiny.org/action-layout.html
# https://shiny.rstudio.com/gallery/

# load packages
library(shiny)
library(ggplot2)
library(dplyr)
library(rsconnect) # ? needed
library(ggmosaic)

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
                     sidebarPanel(
                         selectInput(
                             inputId = "x",
                             label = "Select demographic:",
                             choices = c(
                                 "age",
                                 "sex",
                                 "race",
                                 "year",
                                 "degree"),
                             selected = "age"
                             ),
                         ),
                     mainPanel(plotOutput("barChartDem")),
                     ),
            
            tabPanel("Sleep",
                     fluidRow(
                         mainPanel(plotOutput("sleepDur_enough")),
                     ),
                     fluidRow(
                         sidebarPanel(
                             selectInput(
                                 inputId = "z",
                                 label = "Select question:",
                                 choices = c(
                                     "How would you describe your energy levels?" = "energy",
                                     "How would you describe your stress levels?" = "stress",
                                     "How would you describe your ability to concentrate?" = "concentration",
                                     "How would you describe your overall mood?" = "mood",
                                     "How do you feel you are doing academically?" = "academics"),
                                 selected = "energy"
                             ),
                         ),
                         mainPanel(plotOutput("SleepDur_healthAcad")),
                     ),
                     
                     fluidRow(
                         sidebarPanel(
                             selectInput(
                                 inputId = "y",
                                 label = "Select question:",
                                 choices = c(
                                     "How would you describe your energy levels?" = "energy",
                                     "How would you describe your stress levels?" = "stress",
                                     "How would you describe your ability to concentrate?" = "concentration",
                                     "How would you describe your overall mood?" = "mood",
                                     "How do you feel you are doing academically?" = "academics"),
                                 selected = "energy"
                             ),
                         ),
                         mainPanel(plotOutput("Nap_healthAcad")),
                     ),
                         
                    
                     fluidRow(
                         mainPanel(plotOutput("AllNighter_Acad")),
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
            
            tabPanel("Degree", # which majors sleep more, have better gpa, mental health, etc
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
    output$panel <- renderText({
        paste("Current panel: ", input$tabset)
    })
    
    output$barChartDem <- renderPlot({
        ggplot(rest_habits, aes_string(x=input$x)) + 
            geom_bar() +
            labs(title="Who responded to our survey?", y="Number of Responses") +
            geom_text(aes(label=..count..), stat="count", vjust = -0.2)  
            #coord_flip()
    })
    
    output$sleepDur_enough <- renderPlot({
        rest_habits %>% 
            filter(!is.na(sleepDuration)) %>% 
            ggplot() +
            geom_mosaic(aes(x=product(sleepDuration, sleepEnough), fill=sleepDuration))
    })
    
    output$SleepDur_healthAcad <- renderPlot({
        rest_habits %>% 
            filter(!is.na(sleepDuration)) %>% 
            filter(!is.na(input$z)) %>% 
            mutate(sleepDuration = factor(sleepDuration, levels = level_sleepDur)) %>%
            mutate(input$z = factor(input$z, levels = level_acad)) %>%
            ggplot(aes_string(x = input$z)) + 
            geom_bar(aes(fill = sleepDuration), position = "dodge")
    })
    
    output$Nap_healthAcad <- renderPlot({
        rest_habits %>% 
            filter(!is.na(napFreq)) %>% 
            filter(!is.na(academics)) %>% 
            ggplot(aes_string(x = input$y)) + 
            geom_bar(aes(fill = napFreq), position = "dodge")
    })
    
    output$AllNighter_Acad <- renderPlot({
        rest_habits %>% 
            filter(!is.na(academics)) %>% 
            filter(!is.na(allNighter)) %>% 
            ggplot() +
            geom_mosaic(aes(x=product(allNighter, academics), fill=allNighter))
    })
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






