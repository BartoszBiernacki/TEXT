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


df <- read.csv("http://jsienkiewicz.pl/TEXT/lab/data_fn.csv")
df.corp <- corpus(df)
summary(df.corp, n = 5)



df.mat <- df.corp %>% 
  tokens(remove_punct = T) %>% 
  dfm %>% dfm_remove(stopwords("english")) %>% 
  dfm_wordstem()

df.mat

df.mat %>% topfeatures(n = 20)

col <- sapply(seq(0.1, 1, 0.1), function(x) adjustcolor("blue", x))
df.mat %>% textplot_wordcloud(max_words = 200, color = col)

df.mat %>% topfeatures(n=20, groups = label)

df.mat %>% 
  dfm_group(groups = c("TRUE", "FAKE")[label+1]) %>% 
  textplot_wordcloud(
    max_words = 300,
    comparison = TRUE,
    color = c("red", "darkgreen")
    )


df.trim <- df.mat %>% dfm_trim(min_termfreq = 5, min_docfreq = 5)

df.lda <- df.trim %>% 
  convert(to="topicmodels") %>% 
  LDA(k = 10)

as.data.frame(topics(df.lda, 10))


table(topics(df.lda))

mat <- table(topics(df.lda), df.trim@docvars$label[df.trim@docvars$docid_ %in% names(topics(df.lda))])
mat

row1 <- table(topics(df.lda))
row2 <- mat %>% apply(1, \(x) x/sum(x)) %>% round(2)

df.out <- rbind(row1, row2[2,], as.data.frame(terms(df.lda, 10)))

df.out %>% 
  kbl() %>%
  kable_paper(c("hover", "striped")) %>%
  column_spec(which(df.out[2,] > 0.5), color = "white", background = "orange")


df.s <- df.corp %>% textstat_summary()
df.s[1:10,]


df.s <- df.s %>% mutate(label = c("true", "fake")[df$label + 1]) 
df.s %$% boxplot(chars ~ as.factor(label))


df.lex <- df.mat %>% textstat_lexdiv(measure = c("TTR", "C")) %>% mutate(label = df.s$label)
df.lex[1:10,]

ggplot(df.lex) + geom_boxplot(aes(x = label, y = C, fill = label))


df.read <- df.corp %>% textstat_readability(measure = "FOG")
df.read[1:10,]

df.lexdiv <- df.mat %>% textstat_lexdiv(measure = c("TTR", "C"))
df.red <- df.corp %>% textstat_readability(measure = c("Flesch", "SMOG"))

df.s %>% select(document, label, chars, puncts, numbers, urls) %>% 
  inner_join(df.lexdiv) %>% 
  inner_join(df.red) %>%
  pivot_longer(!c(document, label)) %>%
  ggboxplot(x = "label", y = "value", color = "label") %>% facet(facet.by = "name", scales = "free_y", nrow = 2) +
  stat_compare_means(vjust = 0.5, size = 3) + scale_y_log10()




































