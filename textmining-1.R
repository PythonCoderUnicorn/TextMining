
# ========== TEXT ANALYSIS / TOPIC MODELING

# tutorial by RLadies Freiburg


library(topicmodels)
library(tidytext)
library(tidyverse)
library(gutenbergr)
library(paletteer)


# Associate Press data from topicmodels
data("AssociatedPress")
tidy(AssociatedPress)


# Latent Dirichlet Allocation algorithm, k determines number of topics to be created
AP_LDA = LDA(AssociatedPress,
             k= 2,
             control = list(seed=1234)
             )
AP_LDA

# word topic probabilities of each word to be associated with each topic
# 
ap_topics <- tidy(AP_LDA, matrix = "beta")
ap_topics

ap_topics %>% 
  arrange( desc(beta))


# words with highest beta per topic
ap_topics %>% 
  group_by( topic ) %>% 
  slice_max( beta, n=10) %>% 
  ungroup() %>% 
  arrange(topic, -beta)


# reorder within function
ap_topics %>% 
  group_by( topic ) %>% 
  slice_max( beta, n=10) %>% 
  ungroup() %>% 
  arrange(topic, -beta) %>% 
  mutate(term = reorder_within(term, beta, topic))


# ggplot
ap_topics %>% 
  group_by( topic ) %>% 
  slice_max( beta, n=10) %>% 
  ungroup() %>% 
  arrange(topic, -beta) %>% 
  mutate(term = reorder_within(term, beta, topic)) %>%  # reorder_within() 
  ggplot( 
    aes(x= beta, y= term, fill= factor(topic))
    )+
  geom_col(show.legend = FALSE)+
  scale_fill_paletteer_d(`"calecopal::vermillion"`) +
  ggdark::dark_mode() +
  labs(title = "Topic Modelling",
       subtitle = 'Topics split into 2 (k= 2)',
       y="word term",
       x="probability (beta)"
       )+
  
  facet_wrap(~topic, scales = 'free')+
  scale_y_reordered() # need to call again


# words are not exclusive for each topic
# labels for these topics are likely business and business


# Per-document-per-topic probabilities, matrix= 'gamma'
ap_documents <- tidy(AP_LDA, matrix = "gamma")
ap_documents %>% view()


# for doc 1 the probability of topic being label 1 is 0.248
# for 0.998 of words in doc 19 & 20 belong to topic 1

# get document 18, see what topic topic 2 is (politics)
tidy(AssociatedPress) %>% 
  filter(document == 18) %>% 
  arrange( desc( count))







# books -------------------------------------------------------------------

# use 3 books by titles, k = 3

titles = c("Twenty Thousand Leagues under the Sea", 
            "Pride and Prejudice", 
            "Great Expectations")

books = gutenberg_works(title %in% titles) %>% 
  gutenberg_download(meta_fields = 'title')


books %>% view()

# since the books is all 3 books, need to chop it up into chapters

by_chapter = books %>% 
  mutate( chapter = cumsum( # counter starts at 0
    # search for word chapter/CHAPTER
    str_detect(text, regex("^chapter ", ignore_case = TRUE))
  )) %>% 
  ungroup() %>% 
  filter(chapter > 0) %>% 
  unite(col= document, title, chapter) # create new column 'document'


by_chapter %>% view()


# ----- TOKENIZE WORDS
by_chapter_word = by_chapter %>% 
  unnest_tokens(word, text)

by_chapter_word %>% view()



# --- count words in each chapter, remove STOP WORDS
word_counts = by_chapter_word %>% 
  anti_join(stop_words) %>% 
  count(document, word , sort = TRUE)

word_counts




# document term matrix ----------------------------------------------------

# the LDA needs a dtm, use cast_dtm()
#  each row has a term if not 0 and by chapter

chapters_dtm = word_counts %>% 
  cast_dtm(document = document, word, n)

chapters_LDA = LDA(chapters_dtm, k= 3, control = list(seed=1234))
chapters_LDA

# tidy the LDA
# chapter topics based on word per topic probability
chapter_topics = tidy(chapters_LDA, matrix='beta')
chapter_topics


chapter_topics %>% 
  group_by(topic) %>% 
  slice_max(beta, n= 10) %>% 
  ungroup() %>% 
  arrange( topic, -beta) %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% #view()
  ggplot(
    aes(x= beta, y= term, fill= factor(topic))
  )+
  geom_col(show.legend = FALSE)+
  scale_y_reordered()+
  scale_fill_paletteer_d(`"awtools::ppalette"`)+
  ggdark::dark_mode()+
  facet_wrap(~topic, scales = 'free')


# topic 1 should be Great Expectations
# topic 2 should be twenty thousand leagues
# topic 3 should be pride and prejudice

chapters_gamma = tidy(chapters_LDA, matrix='gamma')
chapters_gamma


# check if LDA makes difference between books
chapters_gamma = chapters_gamma %>% 
  separate(document, c('title','chapter'),
           sep = '_',
           convert = TRUE # chapters as integers
           )
chapters_gamma


chapters_gamma %>%
  ggplot(
    aes(x= factor(topic), y= gamma)
    ) +
  geom_boxplot() +
  facet_wrap(~ title) +
  labs(x = "topic", y = "gamma" ) +
  ggdark::dark_mode()

# we want gamma to be high for the correct book




(chapter_classifications <- chapters_gamma %>%
    group_by(title, chapter) %>%
    slice_max(gamma) %>% # default n is 1
    ungroup())


(book_topics <- chapter_classifications %>%
    count(title, topic) %>%
    group_by(title) %>%
    slice_max(n, n = 1) %>% 
    ungroup() %>%
    transmute(consensus = title, topic))

assignments <- augment(chapters_LDA, 
                       data = chapters_dtm)
assignments


assignments <- assignments %>%
  separate(document, c("title", "chapter"), 
           sep = "_", convert = TRUE) %>%
  inner_join(book_topics, by = c(".topic" = "topic"))
assignments


assignments %>%
  count(title, consensus, wt = count) %>%
  mutate(across(c(title, consensus), 
                ~str_wrap(., 20))) %>% # add line breaks to long titles
  group_by(title) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup() %>% 
  ggplot(
    aes(x= consensus, y=title, fill = percent)
    ) +
  geom_tile() +
  ggdark::dark_mode()+
  scale_fill_paletteer_c(`"grDevices::Warm"`)+
  labs(x = "Book words were assigned to",
       y = "Book words came from",
       fill = "% of assignments")


assignments %>%
  filter(title != consensus) %>%
  count(title, consensus, term, wt = count) %>%
  ungroup() %>%
  arrange(desc(n))




