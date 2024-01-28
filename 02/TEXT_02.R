rm(list = ls())
library(dplyr)
library(tidyverse)
library(gutenbergr)
library(magrittr)
library(tidytext)
library(ggplot2)
library(gridExtra)
library(quanteda)
library(tm)


data("acq")
data.acq <- corpus(acq)
summary(data.acq)

# unikalne tokeny
data.acq %>%
  tokens()

# unikalne tokeny bez znaków przestankowych
data.acq %>%
  tokens(remove_punct = TRUE)

# `DFM term-frequency` documents-features matrix
data.acq %>%
  tokens(remove_punct = TRUE) %>%
  dfm()

# `DFM term-frequency` bez znaków przestankowych i słów funkcyjnych
# ze słowami przepuszczonymi przez stemer
dfm.data <- data.acq %>%
  tokens(remove_punct = TRUE) %>%
  dfm() %>%
  dfm_remove(stopwords("english")) %>%
  dfm_wordstem()

dfm.data

# DFM --> normalna (i transponowana) macierz
dfm.mat <- dfm.data %>%
  as.matrix() %>%
  t()

dfm.mat

# Transformacja TF-IDF (ze słowami funkcyjnymi)
# Bez `scheme_tf = “prop”` była by zwykła suma
dfm.tfidf.data <- data.acq %>%
  tokens(remove_punct = TRUE) %>%
  dfm() %>%
  dfm_wordstem() %>%
  dfm_tfidf(scheme_tf = "prop")

round(t(as.matrix(dfm.tfidf.data)), 4)


# Podobieństwo jako iloczyn skalarny
scalar_product <- function(vec1, vec2){
  return(sum(vec1 * vec2))
}


# Podobieństwo cosinusowe
l2_norm <- function(x){
  return(sqrt(sum(x^2)))
}

cosine_similarity <- function(vec1, vec2){
  numerator <- scalar_product(vec1, vec2)
  denominator <- l2_norm(vec1) * l2_norm(vec2)
  
  return(numerator/denominator)
}

cosine_similarity(vec1=dfm.mat[,1], vec2=dfm.mat[,50])



