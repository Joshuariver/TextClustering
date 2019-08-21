rm(list=ls())

library(dplyr)
library(KoNLP)
library(tidyverse)
library(tidytext)
library(stringr)
library(rvest)
library(extrafont)
library(rJava)
library(translate)
library(googleLanguageR)



# 받은 credential key를 등록합니다.
set.key(AIzaSyDZ3CL940p7KN4XZs5GAcRr2KK_8OlygGQ)

# 번역 함수는 매우 쉽습니다.
translate("안녕하세요? R을 통한 텍스트 마이닝입니다.", "ko", "en", key = get.key())

text <- "to administer medicince to animals is frequently a very difficult matter, and yet sometimes it's necessary to do so"
## translate British into Danish
gl_translate(texts, target = "da")$translatedText


# <YOUR API KEY>
library(translate)

set.key("AIzaSyDZ3CL940p7KN4XZs5GAcRr2KK_8OlygGQ")
translate('hello', 'en', 'de')

texts <- c("안녕하세요. 구글 언어 분석 데모입니다.",
           "한글 분석은 아직 모든 기능을 지원하지는 않습니다.")
nlp_result <- gl_nlp(texts, language = "ko") # 언어를 한국어로 설정하는 것을 잊지 마세요.
str(nlp_result)
