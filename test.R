library(shiny)
library(sotu)
library(tm)
library(wordcloud)

data <- data.frame(sotu_meta,sotu_text)

years <- data$year

getTermMatrix <- function(year){
  index <- match(year,years)

  myCorpus <- Corpus(VectorSource(data$sotu_text[index]))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, stripWhitespace)
  myCorpus = tm_map(myCorpus, removeWords, stopwords("english"))
  myCorpus = tm_map(myCorpus, removeWords, c("will"))

  myDTM <- TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))

  m <- as.matrix(myDTM)

  return(sort(rowSums(m),decreasing=TRUE))
}


test <- getTermMatrix(1790)
head(test)
test
data$sotu_text[1]


myCorpus <- Corpus(VectorSource(data$sotu_text[80]))
myCorpus = tm_map(myCorpus, content_transformer(tolower))
myCorpus = tm_map(myCorpus, removePunctuation)
myCorpus = tm_map(myCorpus, removeNumbers)
myCorpus = tm_map(myCorpus, stripWhitespace)
myCorpus = tm_map(myCorpus, removeWords, stopwords("english"))
myCorpus = tm_map(myCorpus, removeWords, c("will"))

myDTM <- TermDocumentMatrix(myCorpus)

m <- as.matrix(myDTM)
df <- sort(rowSums(m),decreasing=TRUE)
df <- data.frame(word = names(df), freq=as.numeric(df))
#m <-(sort(rowSums(m),decreasing=TRUE))
wordcloud2(df)

v<-m
set.seed(123)
wordcloud(names(v),v,colors = brewer.pal(8,"Dark2"), random.order = F , min.freq = 1, max.words = 250, scale =c(5,0.5))
head(m)
m
str(m)
v
