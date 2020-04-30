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

#Remove stop words (typically 'as, the, of, and', etc.)

#This shows the contents of the dataset 'stop words'. 
data(stop_words)

#The stop_words dataset is used for an anti_join that removes them from the tidy_books tibble.
tidy_books <- tidy_books %>%
  anti_join(stop_words)

#Use dplyr's count function to find the most common words across all books.
tidy_books %>%
  count(word, sort = TRUE) 

#Plot teh frequency of the most common words (greater than 600 instances) using ggplot2. ggplot treats strings as factors (even though
#they're characters in the tibble, so mutate is used to reorder the column in descending order.
library(ggplot2)

tidy_books %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n)) %>%
  filter(n > 600) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


#Project Gutenberg texts. 

library(gutenbergr)

#Select and download texts from Project Gutenberg by their ID. This returns the book id and a section of text for each row
#presumably relating to the page width of the original printed book.

hgwells <- gutenberg_download(c(35, 36, 5230, 159))

#Using 'word' as the column name is convenient because it matches with the column name in the stop_words dataset. This makes the
#anti_join simpler. 

tidy_hgwells <- hgwells %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_hgwells %>%
  
  count(word, sort = TRUE)


#Add some works from the Bronte sisters. 

bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))

tidy_bronte <- bronte %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

#A simple ggplot of my own. Note n doesn't function unless it's preceded by count. 
tidy_bronte %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n)) %>%
  filter(n > 600) %>%
  ggplot(aes(word, n)) +
  geom_col() + 
  coord_flip()

#Grouping by book
library(tidyr)

frequency <- bind_rows(mutate(tidy_bronte, author = "Brontë Sisters"),
                       mutate(tidy_hgwells, author = "H.G. Wells"), 
                       mutate(tidy_books, author = "Jane Austen")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  spread(author, proportion) %>% 
  gather(author, proportion, `Brontë Sisters`:`H.G. Wells`)

library(scales)

# expect a warning about rows with missing values being removed
ggplot(frequency, aes(x = proportion, y = `Jane Austen`, color = abs(`Jane Austen` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Jane Austen", x = NULL)

cor.test(data = frequency[frequency$author == "Brontë Sisters",],
         ~ proportion + `Jane Austen`)

cor.test(data = frequency[frequency$author == "H.G. Wells",], 
         ~ proportion + `Jane Austen`)

  
