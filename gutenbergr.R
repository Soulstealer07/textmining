install.packages("gutenbergr")
library(gutenbergr)
gutenberg_metadata
library(dplyr)
gutenberg_works(author == 'Austen, Jane')
library(stringr)
gutenberg_works(str_detect(author,"Austen"))
#Wuthering_heights has ID 768 it is a novel by Emily Bronte published in 1847 under her pseudonym "Ellis Bell"
wuthering_heights <- gutenberg_download(768)
wuthering_heights
#it is returned as a tibble a type of data frame including two variables id and a character vector of the text
books <- gutenberg_download(c(768,1260),meta_fields = "title")
books
book <- gutenberg_download(768)
#This allows for the addition of another field
books %>% 
  count(title)
#Select a book based on its genere or topic
gutenberg_subjects
#Filter based on a specific subject
gutenberg_subjects %>%
  filter(subject == "Detective and mystery stories")
gutenberg_subjects %>%
  filter(grepl("Holmes, Sherlock",subject))
#Find information about authors
gutenberg_authors



#Analysis of text for WutheringHeights
library(tidytext)
words <- book %>%
  unnest_tokens(word,text)
words
word_counts <- words %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort=TRUE)
word_counts




#Wuthering Heights sentiment analysis
tidy_book<- wuthering_heights %>%
  unnest_tokens(word,text)
tidy_book

clean_tidy_book<-tidy_book %>%
  count(word,sort=TRUE)
clean_tidy_book

positive <- get_sentiments("bing") %>%
  filter(sentiment == "positive")

tidy_book %>%
  semi_join(positive) %>%
  count(word, sort = TRUE)

library(tidyr)
bing <- get_sentiments("bing")
library(textdata)



wuthering_heightssentiment<- tidy_book %>%
  inner_join(get_sentiments("bing") %>%
  count(word, sentiment) %>%
  spread(sentiment, n, fill =0) %>%
  mutate(sentiment = positive - negative))

library(ggplot2)

ggplot(wuthering_heightssentiment,aes(word,sentiment)) +
geom_bar(stat="identity",show.legend = FALSE,col="blue") 

wuthering_heightssentiment_cut<-wuthering_heightssentiment[]

wuthering_heightssentiment
if wuthering_heightssentiment$word = "master" 
str_detect(wuthering_heightssentiment,"master")
