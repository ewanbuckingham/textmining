library(dplyr)
library(tidytext)

##Tidying an Emily Dickinson quote. 
#
#The unnest_tokens() function within the tidytext package breaks down text strings into components.
#These can be individual words,  characters, sentences, paragraphs or ngrams. It removes punctuation
#and - by default - also converts everything to lowercase.

text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

text_df <- tibble(line = 1:4, text = text)

text_df %>%
  unnest_tokens(word, text)

#Tidying the works of Jane Austen
#
#This combines the unnest_tokens function with group_by() and mutate() to split out
#line and chapter numbers. str_detect() returns TRUE if there's a match, which increments 
#the cumsum() return value by 1. 
#
#regex() is used to create a new column containing the chapter number.
#It matches digits (//d) and also the roman numerals ivxlc. I think this is a
#standard pattern since no Jane Austen novel has 100+ chapters. In fact, none in
#this data set use roman numerals at all. 

library(janeaustenr)
library(dplyr)
library(stringr)
library(tidytext)

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

tidy_books <- original_books %>%
  unnest_tokens(word, text)

tidy_books