## Shiny
library(shiny)
library(wordcloud)
library(wordcloud2)
library(colourpicker)
library(tm)
library(memoise)
#membuka file csv
  getTermMatrix <- memoise(function(ppkm) {
  twitter <- read.csv(file="data_ppkm.csv",header=TRUE)
  tweet <- twitter$text
  
  myCorpus = Corpus(VectorSource(tweet))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("")))
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
  })
#UI merupakan bagian yang mengatur tampilan web
ui <- fluidPage(
  titlePanel("Sentimen Analisis Kebijakan PPKM"),
  mainPanel( 
    tabsetPanel(type = "tabs",
                tabPanel("Scatterplot", plotOutput("scatterplot")),
                tabPanel("Data Twitter", DT::dataTableOutput('tbl')), 
                tabPanel("Wordcloud",plotOutput("plot"))
    )
  )
)


# SERVER
# Disinialah tempat dimana data akan dianalisis dan diproses lalu hasilnya akan ditampilkan atau diplotkan pada bagian mainpanel() ui yang telah dijelaskan sebelumnya.
server <- function(input, output,session) {
  twitter <- read.csv(file="data_ppkm.csv",header=TRUE)
  
  
  # Output Data
  output$tbl = DT::renderDataTable({ 
    DT::datatable(twitter, options = list(lengthChange = FALSE)) # data akan ditampilkan dalam beberapa halaman.
  })
  
  #Barplot
  output$scatterplot <- renderPlot({ppkm_dataset<-read.csv("data_ppkm.csv",stringsAsFactors = FALSE)
  review <-as.character(ppkm_dataset$text)
  get_nrc_sentiment('happy')
  get_nrc_sentiment('excitement')
  s<-get_nrc_sentiment(review)
  review_combine<-cbind(ppkm_dataset$text,s)
  par(mar=rep(3,4))
  barplot(colSums(s),col=rainbow(10),ylab='count',main='sentiment analisis')
  }, height=400)
  
  #WordCloud
  
  # Make the wordcloud drawing predictable during a session
  terms <- reactive({
    # Change when the "update" button is pressed...
    #input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(input$selection)
      })
    })
  })
  
  output$plot <- renderPlot({
    v <- terms()
    wordcloud(names(v), v, scale=c(8,0.5),
                  min.freq = 4, max.words=100,
                  colors=brewer.pal(8, "Dark2"))
    #wordcloud(corpus.clean,min.freq = 4,max.words=100,random.order=F,colors=brewer.pal(8,"Dark2"))
  })

  
}

shinyApp(ui = ui, server = server)