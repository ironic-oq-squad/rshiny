library(shiny)
library(shinythemes)

ui <- fluidPage(
            # shinythemes::themeSelector(),
            theme = shinytheme("flatly"),
            fluidPage(
              tags$div(
                h1(tags$strong("Data Mining Group 3: Topic Modeling with Synonyms")),
                br(),
              ),
              sidebarLayout(
                sidebarPanel(width = 3,
                  tags$div(
                    h3("Input Data"),
                  ),
                  fileInput(
                    inputId = "csvFile",
                    label = "Insert .csv File Here",
                    multiple = FALSE,
                    buttonLabel = "Browse...",
                    placeholder = "No input"
                  ),
                  textInput(
                    inputId = "tweetCol",
                    label = "Tweet Column Name",
                    placeholder = "Write Header Name Verbatim Here"
                  ),
                  #tags$div(
                  #  h3("Options"),
                  #  h4("Tweet Cleaning"),
                  #),
                  #checkboxInput(inputId = 'removeStopwords', label = 'Remove common stopwords', value = TRUE),
                  #checkboxInput(inputId = 'demojify', label = 'Remove emojis', value = TRUE),
                  #checkboxInput(inputId = 'removeHashtags', label = 'Remove hashtags', value = TRUE),
                  #checkboxInput(inputId = 'removeNumbers', label = 'Remove numbers', value = TRUE),
                  #checkboxInput(inputId = 'removePunctuation', label = 'Remove punctuation', value = TRUE),
                  #checkboxInput(inputId = 'removeURL', label = 'Remove URLs', value = TRUE),
                  #checkboxInput(inputId = 'removeAt', label = 'Remove "@" sign', value = TRUE),
                  #tags$div(
                  #  h4("Stemming/Lemmatization"),
                  #),
                  #checkboxInput(inputId = 'stem', label = 'Stem words', value = FALSE),
                  #checkboxInput(inputId = 'lem', label = 'Lemmatize words', value = TRUE),
                  #actionButton("execute", "PREPARE DATA", color = 'yellow'),
                  #tags$div(br()),
                ),
                mainPanel(
                  tabsetPanel(
                    tabPanel(tags$strong("Input and Preprocessing Visualizer"),
                             tags$div(
                               h1("Input and Preprocessing Visualizer"),
                               h4("This page is to help users gain a better understanding of how a .csv file is processed for Topic Modeling and BERT Sentiment Analysis."),
                               br(),
                               
                             ),
                             fluidRow(textOutput("dimCsvTable")),
                             fluidRow(textOutput("csvType")),
                             fluidRow(tableOutput("csvTable")),
                    ),
                    tabPanel(tags$strong("Topic Modeling"),
                             plotOutput("maxLikelihoodPlot")         
                    ),
                    tabPanel(tags$strong("vaccineBERT Sentiment Analysis"))
                ),
              ),
            ),
            # FOOTER
            tags$div(
              br(),
              strong("Credits:"),
              p("By Anik Burman, Joshua Fink, Sasha Lioutikova, and Grace Smith", tags$br(), 
                "Supervised by Dr. Johann Gagnon-Bartsch, Juejue Wang, and Heather Johnston", tags$br(),
                "Special thanks to Data Mining Group 2, authors of ", tags$a(href = "https://github.com/julianbernado/vaccineBERT",  "vaccineBERT"), "model.", tags$br(),
                "Big Data Summer Institute, University of Michigan, 2021", tags$br(),
                tags$a(href = "bigdatasummerinstitute.org", "bigdatasummerinstitute.org"),
              ),
              br(),
            ),
        ),
        
      )




# shinyUI(
#  fluidPage(
#    titlePanel("Demo - File Input - Upload multiple files"),
#    sidebarLayout(
#      sidebarPanel(
#        fileInput("file","Upload the file", multiple = TRUE), # fileinput() function is used to get the file upload contorl option
#        helpText("Default max. file size is 5MB"),
#        helpText("Select the read.table parameters below"),
#        checkboxInput(inputId = 'header', label = 'Header', value = TRUE),
#        checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
#        radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),
#        uiOutput("selectfile")
#      ),
#      mainPanel(
#        uiOutput("tb")
#    )
#    
#  )
# ))