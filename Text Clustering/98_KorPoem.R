rm(list=ls())

library(dplyr)
library(tidyverse)
library(rvest)
library(magrittr)
library(stringr)
library(googleLanguageR)
library(syuzhet)
library(ggplot2)

GL_AUTH="~/R/Google Cloud/My First Project-b611b131a07e.json"
gl_auth("~/R/Google Cloud/My First Project-b611b131a07e.json")

base_url <- "http://biblio.endy.pe.kr"

poem_link <- str_c(base_url, "/home") %>%
  read_html() %>%
  html_nodes(xpath="//li//div//a[@class='sites-navigation-link']") %>%
  html_attr("href")

gim_eog_poem <- poem_link[2:14]
yundongju_poem <- poem_link[16:21]
choenamseon_poem <- poem_link[22]
hanyongun_poem <- poem_link[24:35]


get_poem <- function(poem_url) {
  
  base_url <- "http://biblio.endy.pe.kr"
  
  # 주소와 앞에서 추출한 시의 링크를 결합, 시 본문에 접근합니다.
  # 원문 텍스트를 추출합니다.
  poem_body <- str_c(base_url, poem_url) %>%
    read_html() %>%
    html_nodes(xpath="//table[@class='sites-layout-hbox']//tbody//tr//td") %>%
    "["(., length(.)) %>%
    html_children() %>%
    html_text()
  
  # 첫 줄이 제목이고 조금 특이한 whitespace인 \u3000으로 구분되어 있어, regular expression으로 제목을 추출한 다음 나머지를 시 본문으로 삼습니다.
  title <- regmatches(poem_body, regexpr('[ ㄱ-ㅎ가-힣]*[\u3000]', poem_body))
  poem_body <- sub('[ ㄱ-ㅎ가-힣]*[\u3000]', ' ', poem_body)
  
  # 제목에서 두 칸 이상의 빈 공간, 특수문자, 한자 등을 제거합니다.
  title <- str_replace_all(title, pattern="\r", replacement="") %>%
    str_replace_all(pattern="\n", replacement=" ") %>%
    str_replace_all(pattern="[\u3000]", replacement="") %>%
    str_replace_all(pattern="[  ]{2}", replacement="") %>%
    str_replace_all(pattern="[[:punct:]]", replacement=" ") %>%
    str_replace_all(pattern="[\u4E00-\u9FD5○]", replacement="") %>%
    str_trim(side="both")
  
  # 마찬가지로 본문에서 두 칸 이상의 빈 공간, 특수문자, 한자 등을 제거합니다.
  poem_body <- str_replace_all(poem_body, pattern="\r", replacement="") %>%
    str_replace_all(pattern="\n", replacement=" ") %>%
    str_replace_all(pattern="[\u3000]", replacement=" ") %>%
    str_replace_all(pattern="[  ]{2}", replacement="") %>%
    str_replace_all(pattern="[[:punct:]]", replacement=" ") %>%
    str_replace_all(pattern="[\u4E00-\u9FD5○]", replacement="")
  
  return(list(title=title, body=poem_body))
}

gim_eog_date <- function(poem_url) {
  base_url <- "http://biblio.endy.pe.kr"
  
  # 다시 본문을 받아옵니다.
  poem_body <- str_c(base_url, poem_url) %>%
    read_html() %>%
    html_nodes(xpath="//table[@class='sites-layout-hbox']//tbody//tr//td") %>%
    "["(., 1) %>%
    html_children() %>%
    html_text()
  
  # regular expression으로 네자리 숫자를 추출합니다.
  date <- regmatches(poem_body, regexpr('[0-9]{4}', poem_body))
  
  return(date)
}

get_yundongju_poem <- function(poem_url) {
  
  # 윤동주 시인의 시를 scraping하기 위한 module
  
  base_url <- "http://biblio.endy.pe.kr"
  
  poem_body <- str_c(base_url, poem_url) %>%
    read_html() %>%
    html_nodes(xpath="//table[@class='sites-layout-hbox']//tbody//tr//td") %>%
    "["(., 1) %>%
    html_children() %>%
    html_text()
  
  # 제목, 쓰인 날짜를 추출합니다.
  title <- regmatches(poem_body, regexpr('[ ㄱ-ㅎ가-힣\u4E00-\u9FD5]+[\n]{2}', poem_body))
  poem_date <- regmatches(poem_body, regexpr('[\u4E00-\u9FD5○]{4}', poem_body))
  poem_body <- sub('[ ㄱ-ㅎ가-힣\u4E00-\u9FD5]*[\n]{2}', ' ', poem_body)
  
  # 제목을 정리합니다.
  title <- str_replace_all(title, pattern="\r", replacement="") %>%
    str_replace_all(pattern="\n", replacement=" ") %>%
    str_replace_all(pattern="[\u3000]", replacement="") %>%
    str_replace_all(pattern="[  ]{2}", replacement="") %>%
    str_replace_all(pattern="[[:punct:]]", replacement=" ") %>%
    str_replace_all(pattern="[\u4E00-\u9FD5○]", replacement="") %>%
    str_trim(side="both")
  
  # 본문을 정리합니다.
  poem_body <- str_replace_all(poem_body, pattern="\r", replacement="") %>%
    str_replace_all(pattern="\n", replacement=" ") %>%
    str_replace_all(pattern="[\u3000]", replacement="") %>%
    str_replace_all(pattern="[  ]{2}", replacement="") %>%
    str_replace_all(pattern="[[:punct:]]", replacement=" ") %>%
    str_replace_all(pattern="[\u4E00-\u9FD5○]", replacement="")
  
  # 쓰인 날짜가 한문으로 되어 있어, 한글로 바꿉니다.
  # 더 쉬운 방법이 있으면 알려주시면 감사하겠습니다.
  poem_date <- str_replace_all(poem_date, "一", "1") %>%
    str_replace_all("三", "3") %>%
    str_replace_all("四", "4") %>%
    str_replace_all("九", "9") %>%
    str_replace_all("○", "0")
  
  return(list(title=title, body=poem_body, date=poem_date))
}

# Google Translater 를 통한 번역 테스트

# textsen <- gl_translate("안녕하세요? R을 통한 텍스트 마이닝입니다.", target = "en")$translatedText
# textsen

bing_analyse_poems <- function(poem_text) {
  
  # 시 원문을 받아, 명사 추출 후 tidytext의 함수를 통해 정리합니다.
  
  text_tokenized <- get_noun_komoran(poem_text) %>%
    data_frame(poem=.) %>%
    unnest_tokens(word, poem)
  
  # sentiment_lex_bing은 Bing lexicon에 한글 단어를 추가한 것입니다.
  bing <- sentiment_lex_bing %>%
    rename(en = word) %>%
    rename(word = ko)
  
  # 시 테이블에서 겹치는 단어를 bing lexicon에 찾아 붙여줍니다.
  poem_sentiment <- text_tokenized %>%
    inner_join(bing)
  
  # positive는 1점, 아니면 (negative) -1점을 부여합니다.
  poem_sentiment <- poem_sentiment %>%
    mutate(score=ifelse(poem_sentiment$sentiment=="positive", 1, -1))
  
  # 총합을 길이로 나누어 점수를 계산합니다.
  final_score <- (sum(poem_sentiment$score) / length(text_tokenized$word)) * 10
  
  return(final_score)
}

nrc_analyse_poems <- function(nrc_poem) {
  
  # NRC lexicon의 감정 종류입니다.
  sentiments_vec <- c("anger", "anticipation", "disgust", "fear",
                      "joy", "sadness", "surprise", "trust")
  
  sent_list <- list()
  
  # 마찬가지로 명사 추출, tidy text 형태로 바꿉니다.
  text_tokenized <- get_noun_komoran(nrc_poem) %>%
    data_frame(poem=.) %>%
    unnest_tokens(word, poem)
  
  for (sent in sentiments_vec) {
    
    # 각 감정 별로 테이블을 만듭니다.
    nrc_sentiment <- sentiment_lex_nrc %>%
      filter(sentiment == sent) %>%
      rename(en = word) %>%
      rename(word = ko)
    
    # 시 텍스트에서 감정 단어가 있는지 확인, 단어의 수를 셉니다.
    sentiment_in_poem <- text_tokenized %>%
      semi_join(nrc_sentiment) %>%
      count(word, sort=TRUE)
    
    # 각 감정 값을 반환합니다.
    sent_list[sent] <- sum(sentiment_in_poem$n)
  }
  
  return(sent_list)
}

poem_title <- list()
poem_body <- list()
poem_date <- list()
poet <- list()
count <- 1

# 김억 시인
for(poem_url in gim_eog_poem) {
  poem_data <- get_poem(poem_url)
  poem_gim_eog_date <- gim_eog_date(poem_url)
  poem_title[[count]] <- poem_data$title
  poem_body[[count]] <- poem_data$body
  poet[[count]] <- "김억"
  poem_date[[count]] <- poem_gim_eog_date
  count <- count + 1
}

# 제목이 자동으로 수집되지 않으므로, 직접 추가합니다.
poem_title[[1]] <- "시형의 음률과 호흡"
# 작품 "비"에는 지은날이 표기되어 있지 않아, 시집 출판일로 대신합니다.
poem_date[[9]] <- "1929"

# 윤동주 시인
for(poem_url in yundongju_poem) {
  poem_data <- get_yundongju_poem(poem_url)
  poem_title[[count]] <- poem_data$title
  poem_body[[count]] <- poem_data$body
  poet[[count]] <- "윤동주"
  poem_date[[count]] <- poem_data$date
  count <- count + 1
}

# 최남선 시인
poem_data <- get_poem(choenamseon_poem[1])
poem_title[[count]] <- poem_data$title
poem_body[[count]] <- poem_data$body
poet[[count]] <- "최남선"
poem_date[[count]] <- "1908"
count <- count + 1

# 한용운 시인
for(poem_url in hanyongun_poem) {
  poem_data <- get_poem(poem_url)
  poem_title[[count]] <- poem_data$title
  poem_body[[count]] <- poem_data$body
  poet[[count]] <- "한용운"
  # 개별 시가 쓰인 일자가 나와있지 않아, 시집의 출간일로 대신합니다.
  poem_date[[count]] <- "1926"
  count <- count + 1
}

# 다시, 자동으로 수집되지 않는 제목을 수동으로 추가합니다.
poem_title[[29]] <- "나의 길"
# 28번 자료, "고적한 밤"은 중복이므로 삭제합니다.
poem_body <- poem_body[-28]
poem_title <- poem_title[-28]
poem_date <- poem_date[-28]
poet <- poet[-28]

# 자료를 합쳐 data frame을 만듭니다. 처음부터 바로 data frame으로 넣어도 괜찮습니다만, 자료가 들쭉날쭉하여 이쪽을 택했습니다.
poem_df <- data_frame(poet=unlist(poet), title=unlist(poem_title), poem=unlist(poem_body), date=unlist(poem_date))

head(poem_df, 10)


# Bing 감정 점수를 계산합니다.
scores <- sapply(poem_df$poem, bing_analyse_poems)

# NRC 감정 점수를 계산합니다.
nrc_analysis_df <- list()

for(i in 1:length(poem_df$poem)) {
  
  nrc_poem <- poem_df$poem[[i]]
  nrc_poet <- poem_df$poet[[i]]
  nrc_title <- poem_df$title[[i]]
  nrc_date <- poem_df$date[[i]]
  
  nrc_df <- as.data.frame(nrc_analyse_poems(nrc_poem)) %>%
    gather(sentiment, value, anger:trust) %>%
    mutate(poet=nrc_poet,
           date=nrc_date,
           title=nrc_title)
  
  nrc_analysis_df[[i]] <- nrc_df
}

nrc_analysis_df <- do.call(rbind, nrc_analysis_df)

head(nrc_analysis_df, 10)

poet_table <- data.frame(table(poem_df$poet, poem_df$date))
colnames(poet_table) <- c("poet", "date", "Freq")

ggplot(data=poet_table, aes(x=date, y=Freq, fill=factor(poet, levels=c("김억", "윤동주", "최남선", "한용운")))) +
  geom_bar(stat='identity', position='stack') +
  scale_fill_manual(values=RColorBrewer::brewer.pal(6, "Spectral")) +
  labs(y="빈도",
       title="쓰인 시의 연도별 빈도") +
  guides(fill=guide_legend(title="시인")) +
  theme(axis.text.x=element_text(angle=45, size=6, hjust=1),
        text=element_text(family="NanumGothic"))

poet_table <- mutate(poet_table, decade=(as.numeric(as.character(date)) %/% 10) * 10)

head(poet_table, 10)

ggplot(data=poet_table, aes(x=decade, y=Freq, fill=factor(poet, levels=c("김억", "윤동주", "최남선", "한용운")))) +
  geom_bar(stat='identity', position='stack') +
  scale_fill_manual(values=RColorBrewer::brewer.pal(6, "Spectral")) +
  labs(x="연도", y="빈도",
       title="쓰인 시의 10년당 빈도") +
  guides(fill=guide_legend(title="시인")) +
  theme(axis.text.x=element_text(angle=45, size=6, hjust=1),
        text=element_text(family="NanumGothic"))

complete_data_frame_bing <- data_frame(date=as.numeric(unlist(poem_df$date)), score=scores) %>%
  mutate(decade=((date %/% 10) * 10)) %>%
  group_by(decade) %>%
  summarize(score=mean(score))

complete_data_frame_bing

ggplot(data=complete_data_frame_bing) +
  geom_bar(aes(x=decade, y=score, fill=score),
           stat="identity", position="identity") +
  ylim(-1, 1) +
  labs(y="평균 분율 점수", x="년도",
       title="Bing 감정 점수 분율의 막대그래프") +
  guides(fill=guide_legend(title="Bing 점수")) +
  theme(text=element_text(family="NanumGothic"))


ggplot(data=complete_data_frame_bing) +
  geom_line(aes(x=decade, y=score), color="blue") +
  geom_point(aes(x=decade, y=score), size=1.3, color="blue") +
  ylim(-1, 1) +
  labs(y="평균 분율 점수", x="년도",
       title="Bing 감정 점수 분율의 선형그래프") +
  guides(fill=guide_legend(title="Bing 점수")) +
  theme(text=element_text(family="NanumGothic"))


bing_score_by_poet <- data_frame(poet=unlist(poem_df$poet), score=scores) %>%
  group_by(poet) %>%
  summarize(score=mean(score))

ggplot(data=bing_score_by_poet) +
  geom_bar(aes(x=reorder(poet, -score), y=score, fill=poet), stat="identity") +
  scale_fill_manual(values=RColorBrewer::brewer.pal(4, "Set1")) +
  labs(title="가장 부정적인 시를 발표한 시인은?",
       x="점수", y="시인") +
  guides(fill=guide_legend(title="시인")) +
  theme(text=element_text(family="NanumGothic"))

nrc_perdate <- nrc_analysis_df %>%
  mutate(decade=((as.numeric(date) %/% 10) * 10)) %>%
  group_by(decade, sentiment) %>%
  summarise(decadeSum=sum(value)) %>%
  mutate(decadeFreqPerc=decadeSum/sum(decadeSum))

head(nrc_perdate, 10)

# 연도별 감정분율 그래

ggplot(data=nrc_perdate, aes(x=sentiment, y=decadeFreqPerc,
                             fill=sentiment)) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette="Spectral") +
  guides(fill=FALSE) +
  facet_wrap(~ decade, ncol=2) +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=8),
        axis.title.y=element_text(margin=margin(0, 10, 0, 0)),
        plot.title=element_text(size=14)) +
  labs(x="감정", y="빈도 분율",
       title="연도별 감정 분율 막대 그래프") +
  theme(text=element_text(family="NanumGothic"))


# 시인별 감정분율 막대그래프

nrc_perpoet <- nrc_analysis_df %>%
  group_by(poet, sentiment) %>%
  summarise(poetSum=sum(value)) %>%
  mutate(dateFreqPerc=poetSum/sum(poetSum))

ggplot(data=nrc_perpoet, aes(x=sentiment, y=dateFreqPerc,
                             fill=sentiment)) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette="Spectral") +
  guides(fill=FALSE) +
  facet_wrap(~ poet, ncol=2) +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=8),
        axis.title.y=element_text(margin=margin(0, 10, 0, 0)),
        plot.title=element_text(size=14)) +
  labs(y="빈도 분율", x="감정",
       title="시인별 감정 분율 막대 그래프") +
  theme(text=element_text(family="NanumGothic"))


