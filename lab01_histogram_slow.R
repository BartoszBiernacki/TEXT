rm(list = ls())
library(dplyr)
library(tidyverse)
library(gutenbergr)
library(magrittr)
library(tidytext)
library(ggplot2)
library(gridExtra)



text2words <- function(df.text){
  words <- df.text %>%
    unnest_tokens(output = word, input = text) %>%
  
  return(words)
}



clean_words <- function(df.words){
  words <- df.words %>%
    anti_join(stop_words)
  
  return(words)
}



make_word_hist <- function(df.words, n.words){
  g <- df.words %>%
    count(word, sort = TRUE) %>%
    mutate(word1 = reorder(word, n)) %>%
    head(n.words) %>%
    ggplot() +
    geom_col(aes(word1, n)) +
    labs(x = NULL, y = NULL) +
    coord_flip()
  
  return(g)
}


text_verne <- gutenberg_download(3748)
text_kafka <- gutenberg_download(7849)

messy_words_verne <- text2words(text_verne)
messy_words_kafka <- text2words(text_kafka)

clean_words_verne <- clean_words(messy_words_verne)
clean_words_kafka <- clean_words(messy_words_kafka)

n.words = 30
g1 = make_word_hist(messy_words_verne, n.words)
g2 = make_word_hist(messy_words_kafka, n.words)
g3 = make_word_hist(clean_words_verne, n.words)
g4 = make_word_hist(clean_words_kafka, n.words)
axs = grid.arrange(g1, g2, g3, g4)
ggsave("my_plot.png", axs, width = 10, height = 8)

print('Finished!')
