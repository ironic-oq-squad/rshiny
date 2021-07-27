library(shiny)
library(shinythemes)
# use the below options code if you wish to increase the file input limit, in this example file input limit is increased from 5MB to 9MB
# options(shiny.maxRequestSize = 9*1024^2)

library(tm)
library(readr)
library(quanteda)
library(dplyr)
library(textclean)
library(tidytext)
library(stringr)
library(textdata)
library(factoextra)
library(amap)
library(vader)
library(WeightedCluster)
library(cluster)
library(ggfortify)
library(factoextra)
library(ggplot2)
library(Matrix)
library(topicmodels)
library(LDAvis)
library(servr)


server <- function(input, output) {
  
  #### PROCESSING CSV FILE ####
  fileData <- reactive({
    infile <- input$csvFile
    if (is.null(infile)) {
      return(NULL)
    }
    read.csv(infile$datapath, header = TRUE)
  })
  
  output$csvType <- reactive({
    class(as.character(fileData()))
  })
  
  #### TOKENIZE STUFF ####
  
  tok <- reactive({
    fileData() %>% 
    unique() %>%
    corpus(text_field='text') %>% 
    tokens()
  })
  
  samp_dat <- reactive({
    tok() %>% 
    dfm()
  })
  
  names <- reactive({samp_dat()@Dimnames[[2]]})
  
  word_expression <- reactive({
    t(matrix(as.vector(samp_dat()),nrow = nrow(samp_dat()),ncol = ncol(samp_dat())))
  })
  
  #### CREATE DOCUMENT TERM MATRIX ####
  
  dtm <- reactive({
    rownames(word_expression()) = names()
    colnames(word_expression()) = paste0("d_",1:ncol(word_expression()))
    word_mod = word_expression()[1:5, 1:5]
    as(word_mod, "dgCMatrix")
  })
  
  #### MAX LIKELIHOOD PLOT ####
  grid_search_topic_k <- seq(5,80,by = 5)
  
  mod_log_lik <- reactive({
    mod_log_lik = NULL
    
    for (i in grid_search_topic_k) 
    {
      mod = LDA(t(dtm()), k=i, method="Gibbs",
                control=list(alpha=0.5, iter=200, seed=12345, thin=1))
      mod_log_lik = c(mod_log_lik,mod@loglikelihood)
    }
  })
  
  output$maxLikelihoodPlot <- renderPlot({
    plot(grid_search_topic_k,mod_log_lik(),ty = "b",
         xlab = "number of topics (k)",
         ylab = "Log likelihood",
         pch = 19)
    abline(v = grid_search_topic_k[which.max(mod_log_lik)],lty = 2)
  })
  
  
  
  
  
    
  
  
  
  #### OUTPUT TABLE ####
  output$csvTable <- renderTable(fileData())
  
  output$dimCsvTable <- reactive({dim(fileData())})
  
  
  
}
#  
#  ## input$file is a data frame and contains the details around the name, size and temp location of the files uploaded
#  # this reactive output display the content of the input$file dataframe
#  output$filedf <- renderTable({
#    if(is.null(input$file)){return ()}
#    input$file # the file input data frame object that contains the file attributes
#  })
  
#  # Extract the file path for file
#  output$filedf2 <- renderTable({
#    if(is.null(input$file)){return ()}
#    input$file$datapath # the file input data frame object that contains the file attributes
#  })
  
#  ## Below code to display the structure of the input file object
#  output$fileob <- renderPrint({
#    if(is.null(input$file)){return ()}
#    str(input$file)
#  })
#  
#  ## Side bar select input widget coming through renderUI()
#  # Following code displays the select input widget with the list of file loaded by the user
#  output$selectfile <- renderUI({
#    if(is.null(input$file)) {return()}
#    list(hr(), 
#         helpText("Select the files for which you need to see data and summary stats"),
#         selectInput("Select", "Select", choices=input$file$name)
#    )
#    
#  })
  
#  ## Summary Stats code ##
#  # this reactive output contains the summary of the dataset and display the summary in table format
#  output$summ <- renderPrint({
#    if(is.null(input$file)){return()}
#    summary(read.table(file=input$file$datapath[input$file$name==input$Select], 
#                       sep=input$sep, 
#                       header = input$header, 
#                       stringsAsFactors = input$stringAsFactors))})
  
#  ## Dataset code ##
#  # This reactive output contains the dataset and display the dataset in table format
#  
  
#  ## MainPanel tabset renderUI code ##
#  # the following renderUI is used to dynamically generate the tabsets when the file is loaded. 
#  # Until the file is loaded, app will not show the tabset.
#  output$tb <- renderUI({
#    if(is.null(input$file)) {return()}
#    else
#      tabsetPanel(
#        tabPanel("Input File Object DF ", tableOutput("filedf"), tableOutput("filedf2")),
#        tabPanel("Input File Object Structure", verbatimTextOutput("fileob")),
#        tabPanel("Dataset", tableOutput("table")),
#        tabPanel("Summary Stats", verbatimTextOutput("summ")))
#  })
#})