library(dplyr)
library(janeaustenr)
library(tidytext)

#Get the tokens
#count the frequency for each word

book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE)
  
#Group by book. count the total number of words in the books. 

total_words <- book_words %>%
  group_by(book) %>%
  summarise(total = sum(n))

#Join the data together so we have word, frequency, total words for each observation. 

book_words <- left_join(book_words, total_words, by = "book")

library(ggplot2)

ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")



