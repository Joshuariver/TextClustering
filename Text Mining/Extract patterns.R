# Stringr in r 10 data manipulation Tips and Tricks
# https://www.r-bloggers.com/2021/05/stringr-in-r-10-data-manipulation-tips-and-tricks/


rm(list=ls())

library(stringr)


# 1 단어의 길이 Word Length

statement<-c("R", "is powerful", "tool", "for data", "analysis")

statement

str_length(statement)


# 2 단어의 결합  Concatnate

str_c(statement,collapse=" ")

str_c("test",1:10, sep="-")

str_c("test",1:10, sep=",")


# 3 공란 데이터 대체  NA Replace

str_c(c("My Name", NA, "John"),".")

str_replace_na(c("My Name", NA, "John"),".")


# 4 문자 추출 String Extraction

str_sub(statement,1,5)

str_sub(statement, 4,-1)<-"Wow"
statement


# 5. 단어 쪼개기  Split

str_split(statement,pattern=" ")


# 6. 지정 단어 포함 문자열 뽑아내기  Subset

str_subset(colors(),pattern="green")

str_subset(colors(),pattern="^orange|red$")   # orange로 시작하거나 or red로 끝나는 문자열 뽑아내기

list<-c("Hai1", "my 10", "Name 20")
str_extract(list,pattern="[a-z]")   # 소문자로 시작하는 문자열 첫 글자를 뽑아낼 때

str_extract(list,pattern="[a-z]+") # 소문자열 전체를 뽑아낼 때


# 7. html view

str_view(statement,"a.")


# 8. Count

str_count(statement,"[ae]")  # 각 단어에 포함된 a 나 e 의 숫자


# 9. Location

str_locate(statement,"[ae]")


#  10. 소문자/대문자 Lower/Upper case

str_to_lower(statement)
str_to_upper(statement)
str_to_title(statement)

# 데이터에서 특정한 문장의 패턴을 추출할 때

string <- "datascience.com for data science articles"

str_extract(string, "for")

# 데이터에서 숫자로 된 값을 추출할 때

string <- "There are 100 phones over there"

str_extract(string, "\\d+")

# 데이터 벡터에서 문자열을 추출할 때

strings <- c("3 phones", "3 battery", "7 pen") 

str_extract(strings, "[a-z]+")  # 문자만 도출할 때
str_extract(strings, "\\d+") # 숫자를 도출할 때


