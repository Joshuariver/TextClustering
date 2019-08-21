rm(list=ls())

#install.packages("devtools")
# devtools::install_github("ropensci/googleLanguageR")

setwd("~/R/Text Clustering/Text Clustering")


library(googleLanguageR)
GL_AUTH="~/R/Google Cloud/My First Project-b611b131a07e.json"
gl_auth("~/R/Google Cloud/My First Project-b611b131a07e.json")

# text <- "to administer medicince to animals is frequently a very difficult matter, and yet sometimes it's necessary to do so"
## translate British into Danish
# gl_translate(text, target = "ko")$translatedText
# gl_auth("~\My First Project-b611b131a07e.json")


# set.key("AIzaSyDZ3CL940p7KN4XZs5GAcRr2KK_8OlygGQ")
# key=API_KEY(AIzaSyDZ3CL940p7KN4XZs5GAcRr2KK_8OlygGQ)


texts <- c("To the Editor: Last October one of the many kindnesses shown to us by our French friends in Nimes was to take me, a confessed do-it-yourselfer, for a look around the local Castorama. Peter D. Lawrence (''French Supermarkets of Stylish Wares for the Home,'' Travel, Jan. 7) nicely described the excellent examples of design found there, and the special style of some of the European solutions to home improvement needs. I wonder, though, how many readers know that castor is the French word for beaver, referring to the homeowner being ''busy as a beaver''? And that there is a competing chain called Bricorama, taking its name from bricolage, the French word for puttering around?

           As enthusiastic travelers, we Americans generally immerse ourselves in all the subtleties of the culture while having a rather stand-offish relationship with the language. Aren't first-rate travel articles like these a good place to learn the language, too? Not a novel idea; we see it often in The Times. I'm just suggesting that you encourage your writers not to miss these opportunities. ")

textsen <- gl_translate(texts, target = "ko")$translatedText

nlp_result <- gl_nlp(textsen, language = "en") # 언어를 한국어로 설정하는 것을 잊지 마세요.
str(nlp_result)

nlp_result
