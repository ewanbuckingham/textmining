library(tidytext)
library(janeaustenr)
library(dplyr)
library(tidyr)

austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

austen_bigrams %>%
  count(bigram, sort = TRUE)

bigrams_seperated <- austen_bigrams %>% 
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filter <- bigrams_seperated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
  
bigram_counts <- bigrams_filter %>%
  count(word1, word2, sort = TRUE)

bigrams_united <- bigrams_filter %>%
  unite(bigram, word1, word2, sep = " ")

austen_trigrams <- austen_books() %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE) %>%
  unite(trigram, c("word1", "word2", "word3"), sep = " ")

#unite works if you add the column names or if you include a collection with them listed in quotes.

bigrams_filter %>%
  filter(word2 == "street") %>%
  count(book, word1, sort = TRUE)

bigram_tf_idf <- bigrams_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))

#This chart shown, but code not included. Derived from previous chapter.
bigram_tf_idf %>%
  group_by(book) %>%
  top_n(15, tf_idf) %>%
  ungroup() %>%
  mutate(bigram = reorder(bigram, tf_idf)) %>%
  ggplot(aes(bigram, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()

bigrams_seperated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)

AFINN <- get_sentiments("afinn")


#c(word2 = "word") temporarily maps word2 to be word, so that the lhs and rhs have a common column for the join. 
not_words <- bigrams_seperated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE)


library(ggplot2)

not_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, contribution, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment value * number of occurrences") +
  coord_flip()
