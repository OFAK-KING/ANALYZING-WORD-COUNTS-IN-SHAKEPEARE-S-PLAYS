library(shiny)
library(quanteda)
library(ggplot2)
library(wordcloud)
library(memoise)
library(RColorBrewer)
library(shinythemes)

# The list of valid books
books <- list("A Mid Summer Night's Dream" = "summer",
              "The Merchant of Venice" = "merchant",
              "Romeo and Juliet" = "romeo")

# task4: add in getDfm function for pre-processing
getDfm <-  function(book, minterms, stem, punct, ngrams) {
  # check that only one of three books is selected
  if (!(book %in% books))
    stop("Unknown book")
  
  # looks in data sub-folder for the files (e.g., romeo.txt, merchant.txt, summer.txt)
  text <- readLines(sprintf("./data/%s.txt", book), encoding="UTF-8")
  
  # could also pass text column of dataframe instead
  myCorpus <- corpus(text)
  
  # if... else if statement depending on 
  if(ngrams == "unigram"){
    ng = 1
  }else if(ngrams == "both"){
    ng = 1:2
  }else if(ngrams == "bigram"){
    ng = 2
  }
  
  dfm(myCorpus, remove = stopwords('english'), 
      remove_punct = punct, stem = stem, ngrams = ng) %>%
    dfm_trim(min_termfreq = minterms, verbose = FALSE)
}
memoise_getDfm <- memoise(getDfm)
# task6: add in shinythemes, memoize function

ui <- fluidPage(theme = shinytheme("slate"),
  titlePanel("Shakespeare's Plays Word Frequencies"), # Application title
  
  # task1: add in the sidebarLayout with sidebarPanel and mainPanel
  # Sidebar layout with a input and output definitions
  sidebarLayout(position = "left",
    
    # Inputs
    sidebarPanel(
  
  # task2: add in the inputs in the sidebarPanel
      # select input
      selectInput(inputId = "book", 
                  label = "CHOOSE A BOOK:",
                  choices = books, 
                  selected = "Romeo and Juliet"),
      
      checkboxInput(inputId = "stem", 
                    label = "STEM WORDS", 
                    value = FALSE),
      
      checkboxInput(inputId = "punct", 
                    label = "REMOVING PUNCTUATION", 
                    value = TRUE),
      
      radioButtons(inputId = "ngrams", 
                   label = "CHOICE IN N-GRAMS",
                   choices = c("Unigrams only" = "unigram",
                               "Unigrams & Bigrams" = "both",
                               "Bigrams only" = "bigram")),
      
      sliderInput(inputId = "minterms", 
                  label = "MINIMUM FREQUENCY",
                  min = 1, 
                  max = 50, 
                  value = 10),
      
      hr(),
      
      actionButton(inputId = "action", 
                   label = "RERUN")
  
  # task1: within the mainPanel, create two tabs (Word Cloud and Frequency)
  
  # task3: add in the outputs in the sidebarPanel
  
  # task6: and modify your counts output object height to 600px
),
# Output
mainPanel(
  tabsetPanel(
    
    tabPanel("Word Cloud",  plotOutput("cloud")),
    
    tabPanel("Counts", plotOutput("freq",width = "100%", 
                                  height = "600px",
                                  inline = FALSE))
  )
)
)
  )

server <- function(input, output) {
  dfm <- reactive({input$action
    isolate({     
      withProgress({
        setProgress(message = "Processing corpus...")         
        getDfm(book = input$book, 
               stem = input$stem, 
               punct = input$punct, 
               ngrams = input$ngrams, 
               minterms = input$minterms) 
        })
    })
  })
    
  # task5: add in reactivity for getDfm function based on inputs
  output$cloud    <- renderPlot({ 
    v <- dfm()
    textplot_wordcloud(v, 
                       min_size=0.5, 
                       max_size=6,
                       max_words=100,
                       color=brewer.pal(8, "Dark2")) 
    })
  output$freq <- renderPlot({
    v <- dfm()
    dfm_freq <- textstat_frequency(v <- dfm(), n = 50)
    
  dfm_freq$feature <- with(dfm_freq, 
                           reorder(feature, frequency))
  
  ggplot(dfm_freq, aes(feature, frequency)) +
    geom_point()+ 
    coord_flip() + 
    theme(text = element_text(size = 18))
  })
  
}

shinyApp(ui = ui, server = server)
