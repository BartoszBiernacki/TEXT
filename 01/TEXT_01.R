rm(list = ls())

# PRZYKŁAD 3.1
library(tidyverse)
library(tidytext)

data <- tibble(doc_id = 1:3, text = 
                 c("Once upon a time there was a king who wished to have a child.",
                   "Once upon a time, in a faraway land there lived a beautiful princess.",
                   "In the olden time, when wishing was having, there used to live a King."))

data


library(ggplot2)
geom_histogram()

library(dplyr)
library(tidy)

upper.tri()