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


get_books <- function(gut_ids, titles){

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

books <- get_books(
  gut_ids = c(34635, 27081),
  titles = c("Menżerya ludzka", "Sonety krymskie")
)


freq_plot <- books %>%
  corpus() %>%
  tokens(remove_punct = T) %>%
  tokens_remove(stopwords(language = "pl", source = "stopwords-iso")) %>%
  tokens_remove(c("--")) %>%
  tokens_ngrams(n = 2, concatenator = " ") %>%
  dfm() %>%
  quanteda.textstats::textstat_frequency(n = 20, groups = title) %>%
  ggplot() +
  geom_col(
    aes(
      x = frequency,
      y = reorder_within(feature, frequency, group),
      fill = group),
    show.legend = F) + 
  facet_wrap(~group, scales = "free", ncol = 2) +
  scale_y_reordered()


print(freq_plot)
ggsave("Bigrams freq plot.png", plot = freq_plot, width = 10, height = 8)


library(igraph)
library(ggraph)


plot_wordnet <- function(books, id, cutoff_count){
  
  title <- dplyr::filter(books, my_id == id)[1,]$title
  
  graph.bi <- books %>%
    dplyr::filter(my_id == id) %>%
    corpus() %>%
    tokens(remove_punct = T) %>%
    tokens_remove(stopwords(language = "pl", source = "stopwords-iso")) %>%
    tokens_remove(c("--")) %>%
    tokens_ngrams(n = 2, concatenator = " ") %>%
    dfm() %>%
    quanteda.textstats::textstat_frequency() %>%
    as_tibble() %>%
    separate(feature, c("word1", "word2"), " ") %>%
    select(!c(docfreq, rank)) %>%
    graph_from_data_frame()
  
  bi.cut.g <- graph.bi - E(graph.bi)[E(graph.bi)$frequency < cutoff_count]
  bi.cut.g <- bi.cut.g - V(bi.cut.g)[degree(bi.cut.g) < 1]
  
  g <- ggraph(bi.cut.g, layout="kk") + 
    geom_edge_link(aes(width = frequency), color = "darkblue") + 
    scale_edge_width_continuous(range = c(0.5,4)) +
    geom_node_point(size = 5, color = "orange") +
    geom_node_text(aes(label = name), size = 5, nudge_y = 0.5) +
    ggtitle(title)
  
  
  ggsave(
    glue("Wordnet of {title} with cutoff {cutoff_count}.png"),
    plot = g,
    width = 10,
    height = 8
    )
  
  print(g)
  return(g)
}


plot_wordnet(books = books, id = 1, cutoff_count = 3.5)
plot_wordnet(books = books, id = 2, cutoff_count = 1.5)








































