library(dplyr)
library(janeaustenr)
library(tidytext)
library(forcats)
library(gutenbergr)
library(ggplot2)

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

#Try to do the same thing as a column plot
#Need to create the calculated value (tf). 
#Will also need to crop the dataset to just 20 values so they can be displayed. 
#
#included_words <- c("the", "to", "and", "of", "her", "a", "i", "in", "was", "it")
#library(forcats)
#
#book_words <- book_words %>%
#  mutate(tf = n/total) %>%
#  group_by(book) %>%
#  filter(word %in% included_words) %>%
#  mutate(word = fct_reorder(word, desc(tf))) %>%
#  ungroup
#  
#
#  
#ggplot(book_words, aes(word, tf, fill = book)) +
#  geom_col(show.legend = TRUE) + 
#  facet_wrap(~book)
#
#####n.b. This code asks ggplot to do something it can't do. Rank the words by term frequency and then plot
#####smoothly descending curves for each. That's not possible because some words are used more than others (i.e.
##### 'to' is not the most consistently used word. For some books it's 'the'. So the factor reorder doesn't work.)
#####I tried using both mutate(word = reorder(word, tf) and forecats::fct_re_order(word, tf) before realising.)

frequency_by_rank <- book_words %>%
  group_by(book) %>%
  mutate(rank = row_number(), `term frequency` = n/total)

#Plotting Zipf's Law - on a log scale an inversely proportional relationship should have a constant slope. 

#Be careful with quotes. `term frequency` is correct. 'term frequency' works in mutate but not in ggplot and is incorrect.
ggplot(frequency_by_rank, aes(rank, `term frequency`, colour = book)) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()

#Viewing the curve as a series of power law calculations 

rank_subset <- frequency_by_rank %>% 
  filter(rank < 500,
         rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

#Overlaying the fitted power law onto the plots from the books.
frequency_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = book)) + 
  geom_abline(intercept = -0.62, slope = -1.1, color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()


#Binding IDF data from the tidytext library onto the book_words tibble. Dropping columns (total) that won't be used going forwards. 

book_words <- book_words %>%
  bind_tf_idf(word, book, n)

book_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

book_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(book) %>% 
  top_n(15) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()

##Physics textbook analysis using tf-idf (text frequency * inverse document frequency).IDF is calculated from the corpus of content you are analysing. 
library(forecats)

library(gutenbergr)
physics <- gutenberg_download(c(37729, 14725, 13476, 30155), 
                              meta_fields = "author")

plot_physics <- physics_words %>%
  bind_tf_idf(word, author, n) %>%
  mutate(word = fct_reorder(word, tf_idf)) %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan", 
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))
plot_physics %>%
  group_by(author) %>%
  top_n(15, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = author)) + 
  geom_col(show.legend = FALSE) + 
  labs(x = NULL, y = "tf_idf") + 
  facet_wrap(~author, ncol = 2, scales = "free") + 
  coord_flip()









