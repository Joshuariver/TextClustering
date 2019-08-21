# Word Cloud 만들기

rm(list=ls())
setwd("~/R/Text Clustering/Text Clustering")


# install.packages("tm", "SnowballC", "wordcloud", "RcolorBrewer")
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

# 데이터 로드
# filePath <- 'http://datascienceplus.com/wp-content/uploads/2015/08/JEOPARDY_CSV.csv'   -> 인터넷상에 데이터가 있을 때
# text <- readLines(filePath)  -> 텍스트 파일 형태로 있을 때
# jeopQ <- read.csv(filePath, stringsAsFactors = FALSE)  -> csv 형태로 있을 때


filePath <- 'http://datascienceplus.com/wp-content/uploads/2015/08/JEOPARDY_CSV.csv'
text <- readLines(filePath)
jeopQ <- read.csv(filePath, stringsAsFactors = FALSE)

jeopCorpus <- Corpus(VectorSource(jeopQ$Question))
jeopCorpus <- tm_map(jeopCorpus, PlainTextDocument)

inspect(jeopCorpus)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
jeopCorpus <- tm_map(jeopCorpus, toSpace, "/")
jeopCorpus <- tm_map(jeopCorpus, toSpace, "@")
jeopCorpus <- tm_map(jeopCorpus, toSpace, "\\|")

jeopCorpus <- tm_map(jeopCorpus, removePunctuation)
jeopCorpus <- tm_map(jeopCorpus, removeWords, stopwords('english'))
jeopCorpus <- tm_map(jeopCorpus, contents_transformer(tolower))
jeopCorpus <- tm_map(jeopCorpus, removeNumbers)

excludes <- c('the', 'this', stopwords('english'))
jeopCorpus <- tm_map(jeopCorpus, removeWords, excludes)

jeopCorpus <- tm_map(jeopCorpus, stemDocument)

wordcloud(jeopCorpus, max.words = 100, min.freq = 5, random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2")) 
