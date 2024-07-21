# Similarity 


# stringdist 패키지 설치 및 로드
# install.packages("stringdist")
library(stringdist)
library(topicmodels)

# 두 문자열 비교 함수
calculate_similarity <- function(seq1, seq2) {
  # Jaccard 유사도 계산 (또는 다른 방법 선택 가능) eg. cosine
  similarity <- 1 - stringdist(seq1, seq2, method = "jaccard")
  return(similarity)
}

# 두 문자열 비교 예시
seq1 <- "OpenAI is creating amazing AI tools."
seq2 <- "OpenAI creates amazing AI tools."

similarity <- calculate_similarity(seq1, seq2)
print(paste("Similarity ratio:", round(similarity, 2)))
