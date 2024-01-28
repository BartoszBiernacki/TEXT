rm(list = ls())
script_directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(script_directory)

library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(tidyverse)
library(magrittr)
library(topicmodels)
library(kableExtra)
library(ggpubr)


df <-  read.csv("http://jsienkiewicz.pl/TEXT/lab/data_fn.csv")

df.corp <- corpus(df)

df.mat <- df.corp %>% 
  tokens(remove_punct = T) %>% 
  dfm %>% dfm_remove(stopwords("english")) %>% 
  dfm_wordstem()

df.s <- df.corp %>% textstat_summary()
df.s <- df.s %>% mutate(label = c("true", "fake")[df$label + 1])

df.lex <- df.mat %>% textstat_lexdiv(measure = c("TTR", "C"))
df.read <- df.corp %>% textstat_readability(measure = "FOG")

data <- df.s %>%
  inner_join(df.lex) %>% 
  inner_join(df.read) %>%
  dplyr::select(label, chars, tokens, types, sents, puncts, TTR, C, FOG) %>%
  na.omit()


library(MASS)

control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

fit.lda <- train(label~., data=data, method="lda", metric=metric, trControl=control)
fit.knn <- train(label~., data=data, method="knn", metric=metric, trControl=control)
fit.svm <- train(label~., data=data, method="svmRadial", metric=metric, trControl=control)
fit.rf <- train(label~., data=data, method="rf", metric=metric, trControl=control)
fit.nb <- train(label~., data=data, method="naive_bayes", metric=metric, trControl=control)

results <- resamples(list(lda=fit.lda, knn=fit.knn, svm=fit.svm, rf=fit.rf, nb=fit.nb))
print(summary(results)$statistics$Accuracy)









