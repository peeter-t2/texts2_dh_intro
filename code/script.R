#Compiled for R text mining with tidyverse workshop in Rīgas Tehniskā universitāte, April 2019, by Peeter Tinits
#dataset: https://github.com/walkerkq/musiclyrics
#based on: https://en.wikipedia.org/wiki/Billboard_Hot_100
#the concept inspired by work behind Brand, Charlotte; Acerbi, Alberto & Mesoudi, Alex. 2018. Cultural evolution of emotional expression in 50 years of song lyrics. https://osf.io/3j6wx/




#This command installs the libraries needed to run the code, if you don't have them.
lapply(c("tidytext","tidyverse","gridExtra","scales"), 
       function(x) if(!is.element(x, installed.packages())) install.packages(x, dependencies = T))

#Libraries need to be opened each time you open R. These commands open the libraries/packages in the current environment.
library(tidyverse)
library(tidytext)
library(gridExtra)
library(scales)




billboard_data <- read_csv("data/billboard_lyrics_1964-2015.csv")

#basic model is the following
#data %>%
#  process()
#
# %>% - carry the data into function
#select() selecting variables
#filter() provides basic filtering capabilities
#arrange() ordering data
#group_by() groups data by categorical levels
#summarise() summarise data by functions of choice
#join() joining separate dataframes
#mutate() create new variables


# Commands for text processing
# count(variable) - counts the number of items
# top_n(number, variable) - make toplists by variable
# left_join(dataframe) - add one dataframe to another
# unnest_tokens(unit, variable) - make texts into tokens
# str_detect(variable, "string") - partial match of a string



#We can make a variable and view it, by clicking on it on the right,
#or writing view(var)
var <- billboard_data
view(var)

billboard_data %>% 
  count(Artist,sort=T,name="n") %>% 
  top_n(10) %>% 
  ggplot(aes(x=n,y=fct_reorder(Artist,n),fill=Artist))+
  geom_col()


billboard_data %>% 
  filter(Artist%in%c("elton john","janet jackson","whitney houston")) %>% 
  group_by(Year,Artist) %>% 
  summarise(meanyear=mean(Rank)) %>% 
  ggplot(aes(x=Year,y=meanyear,color=Artist))+
  geom_point()+
  geom_line()+
  labs(y="Mean rank per year")+
  scale_y_reverse()



## Trends within lyrics
## Uses tidytext tools https://www.tidytextmining.com/tidytext.html

#Words in song lyrics
billboard_tokens <- billboard_data %>%
  unnest_tokens(word, Lyrics) #new function of unnest_tokens, takes the words from the text, and sets them as separate observations


total_words <- billboard_tokens %>% 
                  count(Year,name="totalcount")

# Position in time

billboard_tokens %>%
  filter(word%in%c("earth","wind","fire")) %>%
  count(word, Year, name="n") %>%
  complete(Year=1964:2015, word, fill=list(n=0)) %>% 
  left_join(total_words) %>%
  ggplot(aes(x=Year, y=n/totalcount, color=word)) +
  geom_point(alpha=0.5) +
  geom_smooth(se=F)


billboard_tokens %>%
  filter(word%in%c("hip","hit","cat")) %>%
  count(word, Year, name="n") %>%
  complete(Year=1964:2015, word, fill=list(n=0)) %>% 
  left_join(total_words) %>%
  ggplot(aes(x=Year, y=n/totalcount, color=word)) +
  geom_point(alpha=0.5) +
  geom_smooth(se=F)

billboard_tokens %>%
  filter(word=="hit") %>%
  count(word, Year, name="n") %>%
  complete(Year=1964:2015, word, fill=list(n=0)) %>% 
  left_join(total_words) %>%
  ggplot(aes(x=Year, y=n/totalcount, color=word)) +
  geom_point(alpha=0.5) +
  geom_smooth()


# Regular expression match

billboard_tokens %>%
  filter(str_detect(word,"^hitch")) %>%
  count(word, Year, name="n") %>%
  complete(Year=1964:2015, word, fill=list(n=0)) %>% 
  left_join(total_words) %>%
  ggplot(aes(x=Year, y=n/totalcount, color=word)) +
  geom_point(alpha=0.5) +
  geom_smooth()



check <- billboard_tokens %>%
  filter(str_detect(word,"[a-z]+ed$")) %>% 
  count(word,sort=T)

billboard_tokens %>%
  filter(str_detect(word,"^(want|tri|start)ed$")) %>%
  count(word, Year, name="n") %>%
  complete(Year=1964:2015, word, fill=list(n=0)) %>% 
  left_join(total_words) %>%
  ggplot(aes(x=Year, y=n/totalcount, color=word)) +
  geom_point(alpha=0.5) +
  geom_smooth(se=F)



# Vocabulary by group

billboard_tokens %>%
  count(word, Year, name="n",sort=T) %>%
  filter(Year==2010) %>%
  mutate(rank=row_number()) %>% 
  top_n(10,n) %>% 
  #complete(Year=1964:2015, word, fill=list(n=0)) %>% 
  #left_join(total_words) %>%
  ggplot(aes(x=Year, y=rank, label=word)) +
  geom_label(alpha=0.5) #+
  #geom_smooth()



billboard_tokens %>%
  count(word, Year, name="n",sort=T) %>%
  filter(Year%in%2000:2010) %>%
  group_by(Year) %>% 
  mutate(rank=row_number()) %>% 
  filter(rank<=10) %>% 
  ggplot(aes(x=Year, y=rank, label=word)) +
  geom_label(alpha=0.5)


top_artists <- billboard_data %>% 
  count(Artist,sort=T,name="artists") %>% 
  top_n(10)


billboard_tokens %>%
  count(word, Artist, name="n",sort=T) %>%
  inner_join(top_artists,by="Artist") %>% 
  arrange(desc(n)) %>%   
  group_by(Artist) %>% 
  mutate(rank=row_number()) %>% 
  filter(rank<=10) %>% 
  ggplot(aes(x=Artist, y=rank, label=word)) +
  geom_label(alpha=0.5)


# Remove stopwords

stopwords <-  stop_words


billboard_tokens %>%
  count(word, Year, name="n",sort=T) %>%
  anti_join(stop_words) %>% 
  filter(Year%in%2000:2010) %>%
  group_by(Year) %>% 
  mutate(rank=row_number()) %>% 
  filter(rank<=10) %>% 
  ggplot(aes(x=Year, y=rank, label=word)) +
  geom_label(alpha=0.5)


billboard_tokens %>%
  count(word, Artist, name="n",sort=T) %>%
  anti_join(stop_words) %>% 
  inner_join(top_artists,by="Artist") %>% 
  arrange(desc(n)) %>%   
  group_by(Artist) %>% 
  mutate(rank=row_number()) %>% 
  filter(rank<=10) %>% 
  ggplot(aes(x=Artist, y=rank, label=word)) +
  geom_label(alpha=0.5)


# Join sentiment words



sentimentwords <-  sentiments


billboard_tokens %>%
  count(word, Year, name="n",sort=T) %>%
  inner_join(sentimentwords) %>% 
  filter(Year%in%2000:2010) %>%
  group_by(Year) %>% 
  mutate(rank=row_number()) %>% 
  filter(rank<=10) %>% 
  ggplot(aes(x=Year, y=rank, label=word,color=sentiment)) +
  geom_label(alpha=0.5)


billboard_tokens %>%
  count(word, Artist, name="n",sort=T) %>%
  inner_join(sentimentwords) %>% 
  inner_join(top_artists,by="Artist") %>% 
  arrange(desc(n)) %>%   
  group_by(Artist) %>% 
  mutate(rank=row_number()) %>% 
  filter(rank<=10) %>% 
  ggplot(aes(x=Artist, y=rank, label=word,color=sentiment)) +
  geom_label(alpha=0.5)





#Looking at sentiments within songs
#Eminem has fairly negative songs throughout
billboard_tokens %>%
  filter(Artist=="eminem") %>%
  group_by(Artist,Song) %>%
  mutate(wordnumber = row_number()) %>%
 # anti_join(stop_words, by = "word") %>%
  mutate(line = wordnumber %/% 20) %>% #Change the number here to group the words into smaller or larger chunks
  inner_join(get_sentiments("bing"), by = "word") %>% 
  count(line,sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative) %>% 
  ggplot(aes(x=line,y=sentiment))+
  geom_bar(aes(fill = sentiment>0),stat = 'identity')+
  theme_minimal()+
  facet_wrap(~Song)

billboard_tokens %>%
  filter(Artist=="the beatles") %>%
  group_by(Artist,Song) %>%
  mutate(wordnumber = row_number()) %>%
  # anti_join(stop_words, by = "word") %>%
  mutate(line = wordnumber %/% 20) %>% #Change the number here to group the words into smaller or larger chunks
  inner_join(get_sentiments("bing"), by = "word") %>% 
  count(line,sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative) %>% 
  ggplot(aes(x=line,y=sentiment))+
  geom_bar(aes(fill = sentiment>0),stat = 'identity')+
  theme_minimal()+
  facet_wrap(~Song)



## Keyword analysis
## tf_idf https://en.wikipedia.org/wiki/Tf%E2%80%93idf
## "term frequency–inverse document frequency"
## reflects how important a word is to a document in a collection or corpus
## Meaning: it finds the words that are special to that text, compared to all other texts in the comparison set
## Read more in https://www.tidytextmining.com/tfidf.html
## http://www.tfidf.com/


billboard_tokens %>%
  count(word, Year, name="n",sort=T) %>%
  bind_tf_idf(term=word, document=Year, n=n) %>% 
  filter(Year%in%2000:2010) %>%
  group_by(Year) %>% 
  arrange(desc(tf_idf)) %>%   
  mutate(rank=row_number()) %>% 
  filter(rank<=10) %>% 
  ggplot(aes(x=Year, y=rank, label=word)) +
  geom_label(alpha=0.5)


billboard_tokens %>%
  count(word, Artist, name="n",sort=T) %>%
  bind_tf_idf(term=word, document=Artist, n=n) %>% 
  inner_join(top_artists,by="Artist") %>% 
  arrange(desc(tf_idf)) %>%   
  group_by(Artist) %>% 
  mutate(rank=row_number()) %>% 
  filter(rank<=10) %>% 
  ggplot(aes(x=Artist, y=rank, label=word)) +
  geom_label(alpha=0.5)

billboard_tokens %>%
  count(word, Year, name="n",sort=T) %>%
  bind_tf_idf(term=word, document=Year, n=n) %>% 
  inner_join(sentimentwords) %>% 
  filter(Year%in%2000:2010) %>%
  group_by(Year) %>% 
  arrange(desc(tf_idf)) %>%   
  mutate(rank=row_number()) %>% 
  filter(rank<=10) %>% 
  ggplot(aes(x=Year, y=rank, label=word, color = sentiment)) +
  geom_label(alpha=0.5)



# Let's look at longer works too

load("data/wells_verne.RData")


jverne_texts <- jverne_texts %>%
  group_by(title) %>%
  mutate(linenumber = row_number()) %>%
  mutate(location=linenumber/max(linenumber)) %>% 
  ungroup()

hgwells_texts <- hgwells_texts %>%
  group_by(title) %>%
  mutate(linenumber = row_number()) %>%
  mutate(location=linenumber/max(linenumber)) %>% 
  ungroup()


jverne_texts %>%
  unnest_tokens(word, text) %>%
  filter(str_detect(word,"god")) %>%
  ggplot(aes(x=location,y=title))+
  geom_point(shape=124)

hgwells_texts %>%
  unnest_tokens(word, text) %>%
  filter(str_detect(word,"god")) %>%
  ggplot(aes(x=location,y=title))+
  geom_point(shape=124)


hgwells_tf_idf <- hgwells_texts %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  group_by(title) %>%
  count(word, sort = TRUE) %>%
  bind_tf_idf(word, title, n) %>%
  arrange(desc(tf_idf)) %>%
  ungroup()


hgwells_tf_idf %>% 
  group_by(title) %>%
  filter(n>10) %>%
  top_n(10) %>%
  ggplot(aes(reorder(word, tf_idf), tf_idf, fill = title)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()+
  guides(fill=FALSE) +
  facet_wrap(~title,scale="free_y")




jverne_tf_idf <- jverne_texts %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  group_by(title) %>%
  count(word, sort = TRUE) %>%
  bind_tf_idf(word, title, n) %>%
  arrange(desc(tf_idf)) %>%
  ungroup()

jverne_tf_idf %>% 
  group_by(title) %>%
  filter(n>10) %>%
  top_n(10) %>%
  ggplot(aes(reorder(word, tf_idf), tf_idf, fill = title)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()+
  guides(fill=FALSE) +
  facet_wrap(~title,scale="free_y")



### Sentiment analysis with locations
##
## A list of words, as last time

hgwellssentiment <- hgwells_texts %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("bing"), by = "word") %>% 
  count(title, index = linenumber %/% 80, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

ggplot(hgwellssentiment, aes(index, sentiment, fill = title)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~title, ncol = 2, scales = "free_x")

jvernesentiment <- jverne_texts %>%
  group_by(title) %>%
  mutate(linenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("bing"), by = "word") %>% 
  count(title, index = linenumber %/% 80, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

ggplot(jvernesentiment, aes(index, sentiment, fill = title)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~title, scales = "free_x")





#
# To get the texts from gutenberg
# 
# library(gutenbergr)
# #The library "gutenbergr" gave us some data to work with, for example "gutenberg_metadata"
# #To look at a variable we just type it in
# View(gutenberg_metadata)
# 
# #To download texts
# #1) build index
# gutenberg_metadata %>%
#   filter(has_text==TRUE) %>%
#   filter(str_detect(author,"Wells, H. G.")) %>%
#   filter(str_detect(language,"en")) -> hgwells_index
# 
# gutenberg_metadata %>%
#   filter(has_text==TRUE) %>%
#   filter(str_detect(author,"Verne, Jules")) %>%
#   filter(str_detect(language,"en")) -> jverne_index
#
#2) downlaod the texts
#hgwells_texts <- gutenberg_download(hgwells_index$gutenberg_id[1:15], meta_fields = "title")
#jverne_texts <- gutenberg_download(jverne_index$gutenberg_id[1:15], meta_fields = "title")
