
# ==== The Qur'an  


# ================ libraries
library(dplyr)
library(tidyr)
library(tidytext)
data("stop_words")
library(stringr)
library(scales)
library(gutenbergr)  # for all free ebooks on Gutenberg Press
library(ggplot2)
library(wordcloud)
library(forcats)
library(igraph)
library(ggraph)
library(topicmodels)

# Alice's Adventures in Wonderland = 10
# The Jungle Book by Rudyard Kipling = 236
# ***** 

# Grimms' Fairy Tales = 2591     {Rapunzel, Snow White, Hansel & Gretel,  MOST OF THE STORIES}

# *****
# Tarzan of the Apes by Edgar Rice Burroughs = 78
# The Jungle Book by Rudyard Kipling = 236
# Notre-Dame de Paris by Victor Hugo = 2610
# The Pied Piper of Hamelin by Robert Browning = 18343
# The Story of Pocahontas and Captain John Smith by E. Boyd Smith = 24487
# The Arabian Nights: Their Best-known Tales by Smith, Wiggin, and Parrish = 20916
# Beauty and the Beast by Jeanne-Marie Leprince de Beaumont = 7074
# The Blue Fairy Book by Andrew Lang = 503 {Cinderella, Sleeping Beauty, Goldilocks}
# Hans Andersen's Fairy Tales. First Series by H. C. Andersen = 32571 {Snow Queen, Thumbelina, Ugly Duckling}
# English Fairy Tales by Joseph Jacobs = 7439 {Jack & Beanstalk}
# Bambi by Marjorie Benton Cooke = 11197
# Peter Pan by J. M. Barrie = 16
# The Adventures of Pinocchio by Carlo Collodi = 500


# little mermaid, cinderella, snow white, sleeping beauty
# rapunzel, beauty & beast, mulan, little red riding, princess & the frog [The Frog Princess], 
# aladdin, tarzan, peter pan, pocahontas, hunchback of notre dame, rapunzel, snow white,
# little mermaid, pinocchio, pied piper, bambi, red riding hood, rumpelstiltskin, alice in wonderland,
# frozen <-- The Snow Queen, Anastasia {very Russian - killing}, thumbelina, 

# fox & the hound [no book avail, Daniel P. Mannix -- story about hunting],




Alice_Wonderland_book = gutenberg_download(10)


book = gutenberg_download(84)

koran = gutenberg_download(2800)

head(koran)

tidy_koran = koran %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

head(tidy_koran)



# most common words / word frequencies
tidy_koran %>% 
  filter(!word %in% c("ye","hath","thy","god","yes","lord","thou","thee",
                      "1","2","3","4","5","6","7","8","9","10","11","12","13")) %>%
  count(word, sort = T) %>% 
  filter(n >90) %>% # get only word counts higher than 600
  mutate(word = reorder(word, n)) %>%  # reorder by word counts, not alphabetically (default)
  ggplot( aes(n, word)) +
  geom_col() +
  labs(y= NULL)


tidy_koran %>% 
  filter(word == "death") %>% 
  count()






























