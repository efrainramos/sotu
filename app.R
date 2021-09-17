

library(shiny)
library(sotu)
library(tm)
library(wordcloud)
library(RColorBrewer)

data <- data.frame(sotu_meta,sotu_text)

years <- data$year

getTermMatrix <- function(year){
    index <- match(year,years)

    myCorpus <- Corpus(VectorSource(data$sotu_text[index]))
    myCorpus = tm_map(myCorpus, content_transformer(tolower))
    myCorpus = tm_map(myCorpus, removePunctuation)
    myCorpus = tm_map(myCorpus, removeNumbers)
    myCorpus = tm_map(myCorpus, removeWords, stopwords("english"))
    myCorpus = tm_map(myCorpus, removeWords, c("will"))
    myDTM <- TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))

    m <- as.matrix(myDTM)

    df <- sort(rowSums(m),decreasing=TRUE)
    df <- data.frame(word = names(df), freq=as.numeric(df))
    return(df[1:200,])
}



ui <- fluidPage(

    # Application title
    titlePanel("United States President's State of The Union Address WordClouds and Sentiment Analysis"),



    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("selection","Choose a year:", choices = years,selected = "2016")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("cloudplot")

        )

    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    term <- reactive({
        term <- getTermMatrix(input$selection)
    })

    output$cloudplot <- renderPlot({

        v<-term()
        wordcloud(words = v$word, freq=v$freq,colors = brewer.pal(8,"Dark2"), min.freq=1, max.words = 250, random.order=F,scale =c(5,0.5))
    })
}

# Run the application
shinyApp(ui = ui, server = server)
