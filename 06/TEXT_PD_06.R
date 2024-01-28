rm(list = ls())
script_directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(script_directory)

library(tidyverse)
library(gutenbergr)
library(tidytext)
library(dplyr)
library(quanteda)
library(MASS)
library(e1071)
library(caret)
library(glue)


data.bbc <- as_tibble(
  read.table(
    "https://jsienkiewicz.pl/TEXT/lab/data_bbc.csv",
    stringsAsFactors = F
    )
  )

data.bbc <- data.bbc %>%
  mutate(emo = ifelse(emo == -1, 1, emo))


data.bbc_emo1 <- data.bbc %>%
  filter(emo == 1) %>%
  slice_sample(n = 500, replace = FALSE)

data.bbc_emo0 <- data.bbc %>%
  filter(emo == 0) %>%
  slice_sample(n = 500, replace = FALSE)

# Combine the selected samples
data.bbc <- bind_rows(data.bbc_emo1, data.bbc_emo0)


data.bbc$text <- sapply(data.bbc$text, enc2utf8)
data.bbc$emo <- as.factor(data.bbc$emo)
data.bbc$doc_id <- 1:nrow(data.bbc)



test_svml_accuracy <- function(dfm_with_emo){
  
  levels(dfm_with_emo$emo) <- c("objective", "subjective")
  data <- cbind(
    convert(dfm_with_emo, to = "data.frame"),
    class.out = dfm_with_emo$emo
  )
  
  fit <- trainControl(method = "cv", number = 5)
  
  model.svml <- train(
    class.out ~ .,
    data = data,
    method = "svmLinear",
    trControl = fit,
    scale = F
  )
  
  accuracy <- mean(model.svml$results$Accuracy)
  sd_accuracy <- mean(model.svml$results$AccuracySD)
  
  print(glue("Accuracy = {format(accuracy, digits = 3)} +- {format(sd_accuracy, digits = 2)}"))
}


bbc.dfm.full <- data.bbc %>%
  corpus() %>%
  tokens(remove_punct = TRUE) %>%
  tokens_remove(stopwords("english")) %>% 
  dfm() %>%
  dfm_wordstem()

bbc.dfm.trimmed <- bbc.dfm.full %>%
  dfm_trim(min_termfreq = 5, min_docfreq = 5)

bbc.dfm.trimmed <- bbc.dfm.trimmed %>%
  dfm_subset(ntoken(bbc.dfm.trimmed) > 0, drop_docid = FALSE)


bbc.df.tfidf.full <- bbc.dfm.full %>%
  dfm_tfidf(scheme_tf = "prop")

bbc.df.tfidf.trimmed <- bbc.dfm.trimmed %>%
  dfm_tfidf(scheme_tf = "prop")


cat(" No trimming, no tfidf\n")
test_svml_accuracy(bbc.dfm.full)

cat("\n No trimming, tfidf\n")
test_svml_accuracy(bbc.df.tfidf.full)

cat("\n trimming, no tfidf\n")
test_svml_accuracy(bbc.dfm.trimmed)

cat("\n trimming, tfidf\n")
test_svml_accuracy(bbc.df.tfidf.trimmed)












