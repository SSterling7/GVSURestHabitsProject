
# https://mastering-shiny.org/action-layout.html
# https://shiny.rstudio.com/gallery/

# load packages
#library(shiny)
library(ggplot2)
library(dplyr)
library(rsconnect) # ? needed
library(ggmosaic)
library(wordcloud2)
library(shinythemes)
library(stringr)

# read in data
rest_habits <- readRDS("data/shinydata.rds") # HAHA got eem doesn't like here::here

# Define UI
ui <- fluidPage(
    theme = shinytheme("journal"),
    # Application title
    titlePanel("Rest Habits at GVSU"),
    # Plot bar chart
    mainPanel(
        tabsetPanel(
            id = "tabset",
            tabPanel("Summary",
                     mainPanel(h4("STA 418/518 Final Project: Rest Habits at GVSU", align="center"),
                               h4("Stella Sterling & Lauren Proctor", align="center"),
                               p(em("Investigating the rest habits of GVSU students and how it relates 
                                 to overall wellbeing and academic performance."), align = "center"),
                               p("Inspired by the Press Pause campaign; part of GVSU Recreation & Wellness’s WIT program. 
                               Press Pause explores vital areas of rest and strives to help students gain healthy rest habits. 
                               Researching student rest habits will help the Welness Information Team gain a better 
                                 understanding of how GVSU students prioritize and utilize rest."),
                               br(),
                               p("Check out ", a("Press Pause", href = "https://www.gvsu.edu/studentwellness/press-pause-59.htm"),
                                 " to learn more about the campaign and for information on how you can improve your rest habits!"),
                               br(),
                               h5("Survey")
                     ),
            ),
            
            tabPanel("Demographics",
                     fluidRow(
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
                                 selected = "age")
                         ),
                         mainPanel(plotOutput("barChartDem")),
                     ),
                     fluidRow(
                         mainPanel(plotOutput("histGPA")),
                     ),
            ),
                     
                     tabPanel("Sleep",
                              fluidRow(
                                  mainPanel(plotOutput("sleepDur_enough")),
                              ),
                              fluidRow(
                                  sidebarPanel(
                                      selectInput(
                                          inputId = "z",
                                          label = "Select question to compare to duration of sleep:",
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
                                          label = "Select question to compare to frequency of naps:",
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
                                  column(5, plotOutput("ltYN_socMedia")),
                                  column(7, plotOutput("lt_socMedia"))
                              ),
                              
                              fluidRow(
                                  sidebarPanel(
                                      selectInput(
                                          inputId = "lt",
                                          label = "Select an option:",
                                          choices = c(
                                              "Frequency of Leisure Time vs GPA" = "leisureFreq",
                                              "Social Media Use vs GPA" = "socMedDuration"),
                                          selected = "leisureFreq"
                                      ),
                                  ),
                                  mainPanel(plotOutput("lt_gpa")),
                              ),
                     ),
                     
                     tabPanel("Degree", # which majors sleep more, have better gpa, mental health, etc
                              fluidRow(
                                  mainPanel(wordcloud2Output("majorCloud")),
                                  ),
                              fluidRow(
                                  mainPanel(p("With great diversity of majors and since GVSU's colleges weren't as informative, 
                                           we decided to sort majors into groups of our own making."),
                                            br(),
                                         p(strong("Here are examples of each group: ")),
                                         p("businessLegal: ", em("Business, management, and legal related fields. (Hospitality and 
                                                                 Tourism Management; Accounting; Criminal Justice)")),
                                         p("humanities: ", em("Cultures and humanities. (French; History; Philosophy)")),
                                         p("science: ", em("Biological, environmental and physical sciences. (Physics; Cell and 
                                                           Molecular Biology; Environmental and Sustainability Studies)")),
                                         p("education: ", em("Education related fields. (Higher Education; School Psychology; Art 
                                                             Education")),
                                         p("art: ", em("Fine arts and design. (Film and Video)")),
                                         p("health: ", em("Health and medicine. (Nursing; Biomedical Sciences; Exercise Science")),
                                         p("mathEngineering: ", em("Mathmatical, engineering, and other tech-related fields. 
                                                                   (Statistics; Electrical Engineering; Computer Science)")),
                                         p("social: ", em("Social and behavioural sciences. (Speech-Language Pathology; Social 
                                                          Work; Psychology")),
                                         br(),
                                         p("Some responses were not sorted into groups. These included student-initiated majors
                                           and those who had more than one major. For the former it would have been smart to 
                                           ask which field the student-initiated major belonged to during the survey step. The second 
                                           issue is something that we plan to adapt to in the future.")
                                         )
                              ),
                              fluidRow(
                                  mainPanel(plotOutput("sleepMajor"))
                              ),
                              fluidRow(
                                  column(6, plotOutput("majorGPA")),
                                  column(6, plotOutput("majorAcad"))
                              )
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
        rest_habits %>% 
            filter(!is.na(race)) %>% 
            filter(!is.na(year)) %>% 
        ggplot(aes_string(x=input$x, fill=input$x)) + 
            geom_bar() +
            labs(title="Who responded to our survey?", y="Number of Responses") +
            geom_text(aes(label=..count..), stat="count", vjust = -0.2) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
            #coord_flip()
    })
    
    output$histGPA <- renderPlot({
        rest_habits %>% 
            filter(!is.na(gpa)) %>% 
            ggplot(aes(x=gpa)) + 
            geom_histogram(color="black", fill="lightcoral") +
            geom_vline(aes(xintercept = mean(gpa)), linetype="dashed")
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
            geom_mosaic(aes(x=product(allNighter, academics), fill=allNighter)) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$ltYN_socMedia <- renderPlot({
        rest_habits %>% 
            filter(!is.na(socMedDuration)) %>% 
            filter(!is.na(leisureTime)) %>% 
            ggplot() +
            geom_mosaic(aes(x=product(leisureTime, socMedDuration), fill=leisureTime)) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    output$lt_socMedia <- renderPlot({
        rest_habits %>% 
            filter(!is.na(socMedDuration)) %>% 
            filter(!is.na(leisureFreq)) %>% 
            mutate(leisureFreq = str_wrap(leisureFreq, width = 20)) %>%  
            ggplot() +
            geom_mosaic(aes(x=product(leisureFreq, socMedDuration), fill=leisureFreq)) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$lt_gpa <- renderPlot({
        rest_habits %>% 
            filter(!is.na(gpa)) %>% 
            filter(!is.na(leisureFreq)) %>% 
            filter(!is.na(socMedDuration)) %>% 
            mutate(leisureFreq = str_wrap(leisureFreq, width = 20)) %>% 
            ggplot(aes_string(x = input$lt, y = "gpa", fill=input$lt)) + 
            geom_boxplot() +
            stat_summary(fun=mean, geom="point", shape=20, size=8, color="grey", fill="grey")
    })
    
    output$majorCloud <- renderWordcloud2({
        rest_habits %>% 
            filter(!is.na(category)) %>% 
            group_by(category) %>% 
            summarize(count = n(),
                      wordFreq = count/129) %>% 
            wordcloud2(size=.4)
    })
    
    output$sleepMajor <- renderPlot({
        rest_habits %>% 
            filter(!is.na(category)) %>% 
            filter(!is.na(sleepDuration)) %>% 
            ggplot() +
            geom_mosaic(aes(x=product(sleepDuration, category), fill=sleepDuration)) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$majorGPA <- renderPlot({
        rest_habits %>% 
            filter(!is.na(gpa)) %>% 
            filter(!is.na(category)) %>% 
            ggplot(aes(x=category, y=gpa, fill=category)) + 
            geom_boxplot() +
            stat_summary(fun=mean, geom="point", shape=20, size=4, color="grey", fill="grey") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$majorAcad <- renderPlot({
        rest_habits %>% 
            filter(!is.na(category)) %>% 
            filter(!is.na(academics)) %>% 
            ggplot() +
            geom_mosaic(aes(x=product(academics, category), fill=academics)) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
}

# Run app
shinyApp(ui = ui, server = server)






