rm(list = ls())
script_directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(script_directory)
library(gutenbergr)
library(dplyr)
library(tidyverse)
library(tidytext)
library(magrittr)
library(ggplot2)
library(glue)
library(quanteda)
library(quanteda.textstats)
library(textdata)

gutenberg_get_mirror()
gutenberg_works()

book <- gutenberg_download(30) # bible

book


nouns <- parts_of_speech %>%
  filter(pos == "Noun")

nouns


frequent_nouns <- book %>%
  unnest_tokens(output = word, input = text) %>%
  anti_join(stop_words) %>%
  inner_join(nouns) %>%
  count(word, sort = T) %>%
  top_n(n = 3, wt = n)

frequent_nouns


bigrams <- book %>%
  corpus() %>%
  tokens(remove_punct = T) %>%
  tokens_remove(stopwords(language = "en")) %>%
  tokens_ngrams(n = 2, concatenator = " ") %>%
  dfm() %>%
  textstat_frequency() %>%
  as_tibble() %>%
  separate(feature, c("word1", "word2"), " ") %>%
  select(!c(docfreq, rank, group))
  
  
bigrams



plot_bigram_sentiment <- function(bigrams, right_word){
  
  nrc <- get_sentiments("nrc")
  
  nrc_categories <- bigrams %>%
    filter(word2 == right_word) %>%
    inner_join(nrc, by = c("word1" = "word") ) %>%
    rename(word = word1) %>%
    mutate(word2 = NULL) %>%
    group_by(sentiment) %>%
    top_n(15, frequency) %>%
    slice(1:15) %>%
    ungroup() %>%
    mutate(word = reorder(word, frequency))
  
  plot <- nrc_categories %>%
    ggplot() +
    geom_col(aes(word, frequency, fill = sentiment), show.legend = FALSE) + 
    coord_flip() + 
    facet_wrap(~sentiment, nrow = 3, scales = "free") +
    ggtitle(right_word)
  
  print(plot)
  
  filename = glue("Sentiment bigram plot for --{right_word}--.png")
  print(filename)
  
  ggsave(filename = filename, plot = plot, width = 10, height = 8)
}


for(noun in frequent_nouns$word){
  plot_bigram_sentiment(
    bigrams = bigrams,
    right_word = noun
  )
}

library(reshape2)
library(wordcloud)


save_wordcloud <- function(bigrams, right_word){
  bing <- get_sentiments("bing")
  
  filename = glue("Wordcloud for --{right_word}--.png")
  print(filename)
  png(filename, width = 1920, height = 1080, units = "px", res=300)
  
  bigrams %>%
    filter(word2 == right_word) %>%
    inner_join(bing, by = c("word1" = "word")) %>%
    rename(word = word1) %>%
    mutate(word2 = NULL) %>%
    acast(
      word ~ sentiment,
      value.var = "frequency",
      fill = 0) %>%
    comparison.cloud(
      colors = c("red", "darkgreen"),
      max.words = 100)
  
  dev.off()
  
}

for(noun in frequent_nouns$word){
  save_wordcloud(
    bigrams = bigrams,
    right_word = noun
  )
}


pos_plot_data <- bigrams %>%
  filter(word2 %in% frequent_nouns$word) %>%
  rename(word = word1) %>%
  left_join(parts_of_speech) %>%
  group_by(word2) %>%
  mutate(nn = frequency / sum(frequency)) %>%
  mutate(word = NULL) %>%
  ungroup() %>%
  group_by(word2, pos) %>%
  summarize(nn = sum(nn)) %>%
  arrange(desc(nn))


plot <- pos_plot_data %>%
  ggplot() +
  geom_bar(
    aes(x = reorder(pos, -nn),
      y = nn,
      fill = reorder(pos, nn)),
    stat="identity"
    ) + 
  coord_flip() +
  theme(legend.position = "none") + 
  labs(y = "fraction", x = "POS") +
  facet_wrap(~word2)


print(plot)
ggsave("POS participation.png", plot = plot, width = 10, height = 8)




































