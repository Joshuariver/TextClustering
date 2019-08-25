# 감성분석(Sentiment Analysis) 적용 분석 예제

setwd("D:/RLab/Sentiment Analysis/KnuSentiLex-master")
library(KoNLP)
library(stringr)

# 

senti_words_kr <- readr::read_delim("SentiWord_Dict.txt", delim='\t', col_names=c("term", "score"))
head(senti_words_kr)
dim(senti_words_kr)
table(senti_words_kr$term)

# 감성어 사전 준비

library(SentimentAnalysis)
x <- duplicated(senti_words_kr$term)
senti_words_kr <- senti_words_kr[!x,]
senti_words_kr[x,]
senti_dic_kr <- SentimentDictionaryWeighted(words=senti_words_kr$term,
                                            scores=senti_words_kr$score)

senti_dic_kr <- SentimentDictionary(senti_words_kr$V1[senti_words_kr$score>0],
                                    senti_words_kr$V1[senti_words_kr$V2<0])
summary(senti_dic_kr)

# 데이터 가져오기

#load("data/twit_bigdata.RData")
# doc <- readLines("ID해외영업_고객가치.txt", encoding = 'UTF-8')
doc <- read_lines('ID해외영업_고객가치.txt')
doc1 = str_replace_all(doc, "[^0-9a-zA-Zㄱ-ㅎㅏ-ㅣ가-힣[:space:]]", " ")
doc1 = str_replace_all
doc1 = str_replace_all(doc1, "[\n\t]", " ")
doc1 = str_trim(doc1)
doc1 = str_replace_all(doc1, "\\s+", " ")

library(tm)
# 벡터를 코퍼스로 변환
corp <- VCorpus(VectorSource(doc1)) # do not use Corpus() instead VCorpus
#content(corp)

# tdm with n-gram tokenizer
tdm <- TermDocumentMatrix(corp, #corp는 corpus, 
                          control=list(wordLengths=c(1,Inf),
                                       tokenize=function(x) SentimentAnalysis::ngram_tokenize(x, char=F)))
tail(Terms(tdm))
head(Terms(tdm))

# 희소단어 제거
tdm_r <- removeSparseTerms(tdm, sparse=0.95)
head(Terms(tdm_r), 20)

# 워드클라우드
wordFreq <- slam::row_sums(tdm_r)
wordFreq <- sort(wordFreq, decreasing=TRUE)
library(wordcloud)
pal <- brewer.pal(8,"Dark2")
w <- names(wordFreq)
wordcloud(words=w, freq=wordFreq, 
          min.freq=3, random.order=F,
          random.color=T, colors=pal)


res_sentiment <- analyzeSentiment(corp, #대신에 corpus,
                                  language="korean",
                                  rules=list("KoreanSentiment"=list(ruleSentiment, senti_dic_kr)),
                                  removeStopwords = F, stemming = F)
res_sentiment
doc1[res_sentiment<0]
rownames(tdm)
