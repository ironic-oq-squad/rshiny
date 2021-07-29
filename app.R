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
library(data.table)
library(qdap)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  fluidPage(
    tags$div(
      h1(tags$strong("Data Mining Group 3: Topic Modeling with Synonyms")),
      h4(tags$a(href = "https://github.com/ironic-oq-squad/rshiny", "Timing out? Clone this RShiny app on GitHub by clicking HERE!"))),
      p("By Anik Burman, Joshua Fink, Sasha Lioutikova, and Grace Smith", tags$br(), 
        "Supervised by Dr. Johann Gagnon-Bartsch, Juejue Wang, and Heather Johnston", tags$br(),
        "Big Data Summer Institute, University of Michigan, 2021", tags$br(),
        tags$a(href = "http://bigdatasummerinstitute.org", "bigdatasummerinstitute.org"),
      ),
      br(),
    ),
    sidebarLayout(
      sidebarPanel(width = 3,
                   tags$div(
                     h3("Input Data"),
                     p("Assumes csv has one column with header 'text'. Adjust the 'Percentage of csv dataset used' to reduce csv dataset size for compuatational purposes.")
                   ),
                   fileInput(
                     inputId = "csvFile",
                     label = "Insert .csv File Here",
                     multiple = FALSE,
                     buttonLabel = "Browse...",
                     placeholder = "No input"
                   ),
                   sliderInput("samplePer", "Percentage of csv dataset used", min = 0, max = 100, val = 10)
      ),
      mainPanel(
        tags$div(
          h3("Maximum Likelihood Plot"),
          p("Finds optimal clusters for LDA Analysis. (Please be patient for program to produce plot)")
        ),
        plotOutput("maxLikelihoodPlot"),
        tags$div(
          h3("LDAVis"),
          p("Visualizes LDA Clusters from Tweets. (Please be patient for program to produce plot)")
        ),
        sliderInput("nTerms", "Number of terms to display", min = 20, max = 40, value = 30),
        visOutput('myChart')
      ),
    ),
)


server <- function(input, output, session) {
  
  #### PROCESSING CSV FILE ####
  fileData <- reactive({
    infile <- input$csvFile
    if (is.null(infile)) {
      return(NULL)
    }
    topic = unique(as.data.frame(read.csv(infile$datapath, stringsAsFactors = FALSE)))
    topic[sample(nrow(topic), (input$samplePer/100)*dim(topic)[1]), ]
    return(topic)
  })
  
  fileCorpus <- reactive({
    if(is.null(fileData())) {
      return(NULL)
    }
    return(corpus(fileData(), text_field = 'text'))
  })
  
  #### TOKENIZE STUFF ####
  samp_dat <- reactive({ 
    if(is.null(fileData())) {
      return(NULL)
    }
    dfm(tokens(unique(fileCorpus()))) 
  })
  
  dtm <- reactive({
    if(is.null(fileData())) {
      return(NULL)
    }
    word_expression = t(matrix(as.vector(samp_dat()), nrow = nrow(samp_dat()), ncol = ncol(samp_dat())))
    names = samp_dat()@Dimnames[[2]] 
    rownames(word_expression) = names 
    colnames(word_expression) = paste0("d_",1:ncol(word_expression)) 
    as(word_expression, "dgCMatrix")
  })
  
  #### MAX LIKELIHOOD PLOT ####
  grid_search_topic_k <- seq(5,80,by = 5) #seq(5, 80, by = 5)
  
  mod_log_lik <- reactive({ 
    if(is.null(fileData())) {
      return(NULL)
    }
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Render Maximum Likelihood Plot", value = 0)
    
    mod_log_lik_in = NULL
    count <- 0
    for (i in grid_search_topic_k) 
    {
      mod = LDA(t(dtm()), k=i, method="Gibbs", control=list(alpha=0.5, iter=200, seed=12345, thin=1))
      mod_log_lik_in = c(mod_log_lik_in, mod@loglikelihood)
      count <- count + 1
      progress$inc(1/16, detail = paste("Iteration ", count, " of 16"))
    }
    return(mod_log_lik_in)
  })
  
  optimalTopicNumber <- reactive ({
    if(is.null(fileData())) {
      return(NULL)
    }
    return(grid_search_topic_k[which.max(mod_log_lik())])
  })
  
  output$maxLikelihoodPlot <- renderPlot({
    if(is.null(fileData())) {
      return(NULL)
    }
    plot(grid_search_topic_k, mod_log_lik(),
         ty = "b",
         xlab = "number of topics (k)",
         ylab = "Log likelihood",
         pch = 19)
    abline(v = optimalTopicNumber(),lty = 2)
  })
  
  text_lda <- reactive({
    if(is.null(fileData())) {
      return(NULL)
    }
    LDA(t(dtm()), k = optimalTopicNumber(), 
        method="Gibbs",
        control=list(alpha=0.5, iter=200, seed=12345))
  })
  
  tm_result <- reactive({
    if(is.null(fileData())) {
      return(NULL)
    }
    tm_result_fun = posterior(text_lda())
    return(tm_result_fun)
  })
  
  output$myChart <- renderVis({
    if(is.null(fileData())) {
      return(NULL)
    }
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Render LDAVis Plot", value = 0)
    
    if(!is.null(input$nTerms)){
      createJSON(phi = tm_result()$terms, 
                 theta = tm_result()$topics, 
                 doc.length = rowSums(t(dtm())), 
                 vocab = colnames(t(dtm())), 
                 term.frequency = colSums(t(dtm())), 
                 R = input$nTerms,
                 plot.opts = list(xlab="", ylab=""))
      progress$inc(1)
    }
  })
}

shinyApp(ui = ui, server = server)
