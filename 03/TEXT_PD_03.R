rm(list = ls())
script_directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(script_directory)
library(gutenbergr)
library(dplyr)
library(tidytext)
library(magrittr)
library(ggplot2)
library(glue)


get_books <- function(gut_ids, titles){
  gutenberg_get_mirror()
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


get_heap_coefs <- function(books){

  words <- books %>%
    unnest_tokens(word, text) %>%
    count(title, my_id, word, sort = TRUE)
  
  total_words <- words %>%
    group_by(title) %>%
    summarise(total = sum(n))
  
  words %<>% left_join(total_words)
  
  books_heaps <- books %>%
    unnest_tokens(word, text) %>% 
    group_by(title, my_id) %>% 
    mutate(M = row_number(), V = cumsum(!duplicated(word)))
  
  books_sum <- books_heaps %>%
    summarise(
      intercept = lm(log10(V) ~ log10(M))$coefficients[1],
      slope = lm(log10(V) ~ log10(M))$coefficients[2]
    )
  
  return(books_sum)
}


get_heap_coefs_shuffled_words <- function(books){
  
  words <- books %>%
    unnest_tokens(word, text) %>%
    count(title, my_id, word, sort = TRUE)

  total_words <- words %>%
    group_by(title) %>%
    summarise(total = sum(n))
  
  words %<>% left_join(total_words)
  
  # Shuffling
  books %<>% unnest_tokens(word, text)
  books <- books[sample(nrow(books)), ]
  
  # Calculating M and V
  books_heaps <- books %>%
    group_by(title, my_id) %>% 
    mutate(M = row_number(), V = cumsum(!duplicated(word)))
  
  # Fitting Heap law
  books_sum <- books_heaps %>%
    summarise(
      intercept = lm(log10(V) ~ log10(M))$coefficients[1],
      slope = lm(log10(V) ~ log10(M))$coefficients[2]
    )
  
  return(books_sum)
}


pl_ids <- c(27081, 27723, 31536, 27208,
            27062, 15201, 34079, 28044)
pl_titles <- c(
  "Sonety Adama Mickiewicza",
  "Moja Pierwsza Bitwa: Opowiadanie Sierżanta",
  "Pan Tadeusz",
  "Threny",
  "Romeo i Julia",
  "Szachy i Warcaby: Droga do mistrzostwa",
  "Tajemnica Baskerville'ów: dziwne przygody Sherlocka Holmes",
  "Kopciuszek: Baśń fantastyczna"
)
pl_books <- get_books(gut_ids = pl_ids, titles = pl_titles)


en_ids <- c(27069, 28277, 28240, 27179,
            2261, 4913, 2852, 10830)
en_titles <- c(
  "Sonnets from the Crimea",
  "My First Battle: A Sergeant's Story",
  "Pan Tadeusz; or, The last foray in Lithuania",
  "Laments",
  "Romeo and Juliet",
  "Chess and Checkers : the Way to Mastership",
  "The Hound of the Baskervilles",
  "Cinderella"
)
en_books <- get_books(gut_ids = en_ids, titles = en_titles)


# Without shuffling
pl_heap_coefs <- get_heap_coefs(books = pl_books)
en_heap_coefs <- get_heap_coefs(books = en_books)

pl_en_heap_coefs <- inner_join(
  pl_heap_coefs,
  en_heap_coefs,
  by = "my_id")

pl_en_heap_coefs %<>%
  select(my_id, title = title.x, beta_pl = slope.x, beta_en = slope.y)

model = lm(pl_en_heap_coefs$beta_en ~ pl_en_heap_coefs$beta_pl)
b = model$coefficients[1]
m = model$coefficients[2]

g <- ggplot(pl_en_heap_coefs, aes(beta_pl, beta_en, color = title)) +
  geom_point(size=10, na.rm = FALSE) +
  theme(legend.position = "bottom") +
  coord_fixed(ratio = 1) +
  xlim(0.5, 1) +
  ylim(0.5, 1) +
  geom_abline(intercept = b, slope = m, col = "red", lty = 2)

# Z jakiegoś dziwnego powodu twierdzi, że dla szachów jest missing value
ggsave("Heap law beta values.png", g, width = 10, height = 8)

result <- cor.test(pl_en_heap_coefs$beta_pl, pl_en_heap_coefs$beta_en)
print(glue("Bez prztasowania: beta_en = {format(m, digits = 3)}*beta_pl + {format(b, digits = 3)}
           wsp.korelacji = {result$estimate}, p_val = {result$p.value}"))


# With shuffling
for (i in 1:3){
  pl_heap_coefs_shuffled <- get_heap_coefs_shuffled_words(books = pl_books)
  en_heap_coefs_shuffled <- get_heap_coefs_shuffled_words(books = en_books)
  
  pl_en_heap_coefs_shuffled <- inner_join(
    pl_heap_coefs_shuffled,
    en_heap_coefs_shuffled,
    by = "my_id")
  
  pl_en_heap_coefs_shuffled %<>%
    select(my_id, title = title.x, beta_pl = slope.x, beta_en = slope.y)
  
  model = lm(pl_en_heap_coefs_shuffled$beta_en ~ pl_en_heap_coefs_shuffled$beta_pl)
  b = model$coefficients[1]
  m = model$coefficients[2]
  
  result <- cor.test(pl_en_heap_coefs_shuffled$beta_pl, pl_en_heap_coefs_shuffled$beta_en)
  print(glue("Po przetasowaniu: beta_en = {format(m, digits = 3)}*beta_pl + {format(b, digits = 3)}
           wsp.korelacji = {result$estimate}, p_val = {result$p.value}"))
  
}







