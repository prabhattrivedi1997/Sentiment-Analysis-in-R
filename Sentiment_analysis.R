rm(list=ls())
library(tidytext)
library(dplyr)
library(janeaustenr)
library(stringr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(wordcloud)
data<-austen_books() %>%    #1st we need to split the multiple words into each row of the novel into the data format of one word per row
                            #%>% will return a string
  group_by(book) %>%      
  mutate(linenumber=row_number(),  #mutate() adds new variables and preserves existing ones
         chapter=cumsum(str_detect(text,regex("^chapter [\\divxlc]",
                                              ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word,text)  ##the function unnest_tokens splts the table into one-token-per-row. This function supports non-standard 
positive_senti<-get_sentiments("bing") %>%
  filter(sentiment=="positive")
  data %>%
  filter(book=="Emma") %>%
  semi_join(positive_senti) %>%   #return all rows from x where there are matching values in y, keeping just columns from x.
  count(word,sort=TRUE)
  bing<-get_sentiments("bing")
Emma_sentiment<-data %>%
   inner_join(bing) %>%
  count(book="Emma",index=linenumber %/% 80, sentiment) %>%
   spread(sentiment,n,fill=0) %>%
   mutate(sentiment=positive-negative)
ggplot(Emma_sentiment,aes(index,sentiment,fill=book))+
  geom_bar(stat="identity",show.legend = TRUE)+
  facet_wrap(~book,ncol=2,scales="free_x")
counting_words<-data %>%
  inner_join(bing) %>%
  count(word,sentiment,sort=TRUE)
head(counting_words)
counting_words %>%
  filter(n>150) %>%
  mutate(n=ifelse(sentiment=="negative",-n,n)) %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(word,n,fill=sentiment))+
  geom_col()+
  coord_flip()+
  labs(y="Sentiment Score")
win.graph(10,10,0.2)
b<-data %>%
  inner_join(bing) %>%
  count(word,sentiment, sort=TRUE)  %>%
   acast(word~sentiment,value.var = "n",fill=0) %>%
   comparison.cloud(colors=c("red","dark green"),max.words=100)

