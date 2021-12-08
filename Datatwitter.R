#install.packages(SnowballC)
#install.packages(Rstem)
#install.packages(twitteR)
#install.packages(tm)
#install.packages(NLP)
#install.packages(SentimentAnalysis)
#install.packages(plyr)
#install.packages(ggplot2)
#install.packages(RColorBrewer)
#install.packages(wordcloud)
#install.packages(sentiment)
library(syuzhet)
library(SnowballC)
library(twitteR)
library(tm)
library(NLP)
library(SentimentAnalysis)
library(plyr)
library(ggplot2)
library(RColorBrewer)
library(ROAuth)
library(rtweet)
library(wordcloud2)
library(wordcloud)

#Mengisi data API Twitter
reqURL <- "http://api.twitter.com/oath/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
CUSTOMER_KEY <- "DGZvtte3bhJEqkbkpAakOr2P2" 
ACCESS_TOKEN <- "449788151-04jg3OPu5Q764c1qS5uQPFJ31Mk9oDtCLY6BYaFy" 
CUSTOMER_SECRET <- "mFFXxfc0oDcVFqKK1YpctDYZz1nFbjLC1YoxQ9tiklHzXqRWtp" 
ACCESS_secret <- "DKGuDuXW4zqhzcPt1biBeV8uciGZafwV1gkcU42YY76Kg" 
setup_twitter_oauth(CUSTOMER_KEY, CUSTOMER_SECRET, ACCESS_TOKEN, ACCESS_secret)

#Mengambil data dari Twitter
ppkm<-searchTwitter("ppkm", n=2000, 
                    retryOnRateLimit = 10e3)

#Menyimpan data awal hasil scrap dari Twitter
saveRDS(ppkm,file = 'data_awal.rds')

#Membuka data awal 
ppkm<-readRDS('data_awal.rds')
data=twListToDF(ppkm)
View(data)

##visualisasi time series 
ts_plot(data, "1 hour") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frekuensi tweet tentang Kebijakan PPKM",
    subtitle = "Twitter status (tweet) counts aggregated using one-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

#Melihat data awal
View(data)

#cleaning data
#hanya ambil tweet saja
komen<-data$text
komenc<-Corpus(VectorSource(komen))

#Menghapus tanda baca, link url, huruf aneh, dan emoji
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
twitclean <- tm_map(komenc, removeURL)

removeNL <- function(y) gsub("\n", " ", y)
twitclean <- tm_map(twitclean, removeNL)

replacecomma <- function(y) gsub(",", "", y)
twitclean <- tm_map(twitclean, replacecomma)

removeRT <- function(y) gsub("RT ", "", y)
twitclean <- tm_map(twitclean, removeRT)

removetitik2 <- function(y) gsub(":", "", y)
twitclean <- tm_map(twitclean, removetitik2)

removetitikkoma <- function(y) gsub(";", " ", y)
twitclean <- tm_map(twitclean, removetitikkoma)

removetitik3 <- function(y) gsub("p…", "", y)
twitclean <- tm_map(twitclean, removetitik3)

removeamp <- function(y) gsub("&amp;", "", y)
twitclean <- tm_map(twitclean, removeamp)

removeUN <- function(z) gsub("@\\w+", "", z)
twitclean <- tm_map(twitclean, removeUN)

remove.all <- function(xy) gsub("[^[:alpha:][:space:]]*", "", xy)
twitclean <- tm_map(twitclean,remove.all)

twitclean <- tm_map(twitclean, removePunctuation)
twitclean <- tm_map(twitclean, tolower)

#membuat nilai untuk masing-masing kata
{
  dtm<-TermDocumentMatrix(twitclean)
  m<-as.matrix(dtm)
  v<-sort(rowSums(m),decreasing = TRUE)
  data<-data.frame(word=names(v),freq=v)
}
head(data,n=10)

wordcloud2(data,shape = "star",
           backgroundColor = "black",
           color = 'random-light',
           size = 0.5)

## save data
dataframe<-data.frame(text=unlist(sapply(twitclean,'[')),stringsAsFactors = F)
View(dataframe)

write.csv(dataframe , "data_ppkm.csv")

###################
#read file
text_df<-read.csv("data_ppkm.csv", stringsAsFactors = FALSE)

#convert ke chracter vector
review <-as.character(text_df$text)

#sentiment scores
get_nrc_sentiment('happy')
get_nrc_sentiment('excitement')
s<-get_nrc_sentiment(review)

#combine text and sentiment columns
review_sentiment<-cbind(text_df$text,s)
View(review_sentiment)

#bar plot for sentiment
barplot(colSums(s),col = rainbow(10), ylab = 'Count', main = 'Sentiment Score For Kebijakan PPKM')

