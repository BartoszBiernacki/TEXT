rm(list = ls())
script_directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(script_directory)

library(tidyverse)
library(gutenbergr)
library(tidytext)
library(dplyr)
library(quanteda)
library(glue)
library(textmineR)


get_tibble_books <- function(gut_ids, titles){
  
  g <- gutenberg_works()
  txts <- gutenberg_download(gut_ids)
  
  gut_id_title <- tibble(
    my_id = 1:length(gut_ids),
    gutenberg_id = gut_ids,
    title = titles
  )
  
  books <- txts %>% 
    left_join(gut_id_title) %>%
    mutate(gutenberg_id = NULL)
  
  return(books)
}


tibble_books_to_corpus <- function(books){
  result <- books %>%
    group_by(title) %>%
    summarise(text = paste(text, collapse = " ")) %>%
    corpus()
}


plot_lda_topics <- function(plot_title, lda_model){
  
  plot <- as_tibble(lda_model$phi) %>% 
    mutate(topic =  as_factor(row_number())) %>% 
    pivot_longer(!topic) %>%
    group_by(topic) %>%
    top_n(10, value) %>%
    mutate(name = reorder_within(name, value, topic)) %>%
    ggplot(aes(name, value, fill = topic)) + 
    geom_col() + 
    facet_wrap(~topic, scale = "free") + 
    scale_x_reordered() + 
    coord_flip() +
    ggtitle(glue("{plot_title}, koherencja = {mean(lda_model$coherence)}")) +
    theme(plot.title = element_text(hjust = 0.5))
  
  print(plot)
  ggsave(filename = glue("{plot_title}.png"), plot = plot, width = 10, height = 8)
}



dumas_books <- get_tibble_books(
  gut_ids = c(1257, 965, 1259),
  titles = c("The three musketeers", "The black tulip", "Twenty years after")
)

austen_books <- get_tibble_books(
  gut_ids = c(105, 158, 161),
  titles = c("Persuasion", "Emma", "Sense and Sensibility")
)

merged_books <- bind_rows(dumas_books, austen_books)



austen_dfm <- austen_books %>%
  tibble_books_to_corpus() %>%
  tokens(remove_punct = TRUE) %>%
  dfm() %>%
  dfm_remove(stopwords("english")) %>%
  dfm_wordstem()

austen_dfm


dumas_dfm <- dumas_books %>%
  tibble_books_to_corpus() %>%
  tokens(remove_punct = TRUE) %>%
  dfm() %>%
  dfm_remove(stopwords("english")) %>%
  dfm_wordstem()

dumas_dfm


merged_dfm <- merged_books %>%
  tibble_books_to_corpus() %>%
  tokens(remove_punct = TRUE) %>%
  dfm() %>%
  dfm_remove(stopwords("english")) %>%
  dfm_wordstem()

merged_dfm


plot_title = "Skład tematów w książkach Dumas"
dumas_lda <- FitLdaModel(dumas_dfm, k = 2, iterations = 100)
plot_lda_topics(plot_title = plot_title, lda_model = dumas_lda)


plot_title = "Skład tematów w książkach Austen"
austen_lda <- FitLdaModel(austen_dfm, k = 2, iterations = 100)
plot_lda_topics(plot_title = plot_title, lda_model = austen_lda)


plot_title = "Skład 2 tematów w zbiorze książek"
merged_lda2 <- FitLdaModel(merged_dfm, k = 2, iterations = 100)
plot_lda_topics(plot_title = plot_title, lda_model = merged_lda2)

plot_title = "Skład 3 tematów w zbiorze książek"
merged_lda3 <- FitLdaModel(merged_dfm, k = 3, iterations = 100)
plot_lda_topics(plot_title = plot_title, lda_model = merged_lda3)















