rm(list = ls())
script_directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(script_directory)
library(dplyr)
library(tidyverse)
library(gutenbergr)
library(magrittr)
library(tidytext)
library(ggplot2)
library(gridExtra)
library(quanteda)
library(tm)
library(ggpp)

# Funkcje pomocnicze ===========================================

# Iloczyn skalarny
scalar_product <- function(vec1, vec2){
  return(sum(vec1 * vec2))
}

# Długość wektora
l2_norm <- function(x){
  return(sqrt(sum(x^2)))
}

# Podobieństwo cosinusowe
cosine_similarity <- function(vec1, vec2){
  numerator <- scalar_product(vec1, vec2)
  denominator <- l2_norm(vec1) * l2_norm(vec2)
  
  return(numerator/denominator)
}
# =============================================================


data("acq")
data.acq <- corpus(acq)


# `DFM term-frequency` bez znaków przestankowych i słów funkcyjnych
# ze słowami przepuszczonymi przez stemer
dfm.data <- data.acq %>%
  tokens(remove_punct = TRUE) %>%
  dfm() %>%
  dfm_remove(stopwords("english")) %>%
  dfm_wordstem()


# Transformacja TF-IDF
dfm.true_tfidf.data <- dfm.data %>%
  dfm_tfidf(scheme_tf = "prop")

# Transformacja TF-IDF ala suma
dfm.sum_tfidf.data <- dfm.data %>%
  dfm_tfidf(scheme_tf = "count")


similarity_values <- function(similarity_function, matrix_){
  similarities <- list()
  most_common_pair <- c(0, 0)
  similarity_max <- 0
  
  ncols <- dim(matrix_)[2]
  
  for (idx1 in 1:(ncols-1)){
    for (idx2 in (idx1+1):ncols){
      vec1 = matrix_[,idx1]
      vec2 = matrix_[,idx2]
      
      similarity = similarity_function(vec1, vec2)
      similarities <- append(similarities, similarity)
      
      if(similarity > similarity_max){
        most_common_pair <- c(idx1, idx2)
        similarity_max <- similarity
      }
    }
  }
  
  result = list(
    similarities = unlist(similarities), # return similaritties as vector not list
    most_common_pair = most_common_pair
    )
  
  return(result)
}


make_hist <- function(dfm.tfidf.data, similarity_function){
  result <- dfm.tfidf.data %>%
    as.matrix() %>%
    t() %>%
    similarity_values(similarity_function, .)
  
  g <- data.frame(Similarities = result$similarities) %>%
    ggplot() +
    geom_histogram(aes(x = Similarities)) +
    labs(x = NULL, y = NULL) +
    scale_x_log10() + 
    annotate(
      "text", x = Inf, y = Inf, vjust = 1, hjust = 1, size=4,
      label = paste("Najbardziej podobne wiadomości to: ",
                    result$most_common_pair[[1]], " i ",
                    result$most_common_pair[[2]]
                    )
             )
  
  return(g)
}

g1 <- make_hist(dfm.true_tfidf.data, scalar_product) +
  ggtitle("TF-IDF + iloczyn skalarny")

g2 <- make_hist(dfm.true_tfidf.data, cosine_similarity) +
  ggtitle("TF-IDF + podobieństwo cosinusowe")

g3 <- make_hist(dfm.sum_tfidf.data, scalar_product) +
  ggtitle("suma słów + iloczyn skalarny")

g4 <- make_hist(dfm.sum_tfidf.data, cosine_similarity) +
  ggtitle("suma słów + podobieństwo cosinusowe")

axs = grid.arrange(g1, g2, g3, g4)
ggsave("TF-IDF.png", axs, width = 10, height = 8)

print('Finished!')






























































