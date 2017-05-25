install.packages("tm")
library(tm)
install.packages("SnowballC")
library(SnowballC)
install.packages("wordcloud")
library(wordcloud)

# Loading data
tweets = read.csv("tweets.csv", stringsAsFactors=F)
# Preprocessing the data
corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c(stopwords("english")))
# Build a document-term matrix out of the corpus
frequencies = DocumentTermMatrix(corpus)
#
allTweets = as.data.frame(as.matrix(frequencies))
colnames(allTweets) <- make.names(colnames(allTweets))
str(allTweets)

ncol(allTweets)

wordcloud(colnames(allTweets),colSums(allTweets),scale=c(2,0.5),min.freq=20,colors=brewer.pal(8, "Dark2"))

wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25), random.order=FALSE, colors=c("red", "green", "blue"))

# Pre-process the corpus, this time removing the most frequent word
corpus <- Corpus(VectorSource(tweets$Tweet))
# 2) Convert the corpus to lowercase 
corpus <- tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
# 3) Remove punctuation from the corpus
corpus <- tm_map(corpus, removePunctuation)
# 4) Remove all English-language stopwords and the most frequent word
corpus <- tm_map(corpus, removeWords, c("apple",stopwords("english")))
# 5) Build a document-term matrix out of the corpus
dtm <- DocumentTermMatrix(corpus)
# 6) Convert the document-term matrix to a data frame called allTweets
allTweets <- as.data.frame(as.matrix(dtm))
colnames(allTweets) <- make.names(colnames(allTweets))
wordcloud(colnames(allTweets),colSums(allTweets),scale=c(2,0.25),colors=brewer.pal(8, "Dark2"))

# Which word cloud is based only on the negative tweets (tweets with Avg value -1 or less)?
allTweets$Avg <- tweets$Avg
# Avg that is negative implies that the data contains negative words negativeTweets = subset(allTweets, tweets$Avg <= -1)

negativeTweets = subset(allTweets, tweets$Avg <= -1)

wordcloud(colnames(allTweets), colSums(negativeTweets))

?wordcloud
