#This code scrapes off the data from websites but you require the websites URL
#This code does
#install.packages("rvest") 
#install.packages('xml2')
#install.packages("lubridate")
library(tidyverse)
library(tidytext)
library(tidyr)
library(igraph)
library(dplyr)
library(stringr)
library(readxl)
library(textreadr)
library(pdftools)
library(magrittr)
library(rvest)
library(reshape2)
library(wordcloud)
library(ggplot2)
library(lubridate)

# Reading all news on Stocks
marketwatch_stocks <- read_html(
  "https://www.marketwatch.com/search?q=stocks&m=Keyword&rpp=15&mp=806&bd=false&rs=false"
)

# Understanding the dates in which the posts were made
stocktimes <- marketwatch_stocks %>%
  html_nodes("div.deemphasized span") %>% #See HTML source code for data within this tag
  html_text()

stocktimes

# Filter datetimes that do not follow a consistent format
#this was taken from an outside framework
stocktimes2 <- c()
for(i in stocktimes){
  correct_stocktimes <- grep("Today", i, invert=T, value=T)
  stocktimes2 <- append(stocktimes2, correct_stocktimes)
}
stocktimes <- stocktimes2

stocktimes

#Turning stocktimes into a standard format
stocktimes_tidy <- gsub("\\.","",stocktimes)

stocktime_final <- parse_date_time(
  stocktimes_tidy, "%I:%M %p %m/%d/%Y"
)
stocktime_final

#Coverting it into Pacific time
stocktime_pacific <- with_tz(
  stocktime_final, "US/Pacific"
)
stocktime_pacific

#Creating a list of URLs for all the news
urls <- marketwatch_stocks %>%
  html_nodes("div.searchresult a") %>%
  html_attr("href")
urls

# Create a dataframe containing the urls of the web 
# pages and their converted datetimes
marketwatch_stock_times <- data.frame(
  WebPg=urls, DateTime=stocktime_pacific
)
#cleans the data so that only articles that have a valid webpage can be accessed
market_clean <- marketwatch_stock_times[grep("http", marketwatch_stock_times$WebPg),]
dim(market_clean)

#Taking the difference of time to better filter out data from certain times
#this is then added as a column to the data frame
diff_in_hours <- difftime(
  Sys.time(), market_clean$DateTime, units = "hours"
)
diff_in_hours
#convert into numeric format
diff_in_hours <- as.double(diff_in_hours)
diff_in_hours
#add to the column
market_clean$DiffHours <- diff_in_hours
head(market_clean)
rownames(market_clean)<-NULL
market_clean
# Filtering the data based on the past 7 hours (1/4th of a day)
#Normally due to the high volume of data, this makes it for a lot of articles
market7hrs <- subset(
  market_clean, DiffHours < 7
)
market7hrs

# Loop through web pg URLs, read and grab the title 
# and body text, and store in a dataframe to get 
# the data ready for analysis
titles <- c()
bodies <- c()
for(i in market7hrs$WebPg){
  
  marketwatch_latest_wbpg <- read_html(i)
  title <- marketwatch_latest_wbpg %>%
    html_node("title") %>%
    html_text()
  titles <- append(titles, title)
  
  marketwatch_latest_wbpg <- read_html(i)
  body <- marketwatch_latest_wbpg %>%
    html_nodes("p") %>%
    html_text()
  one_body <- paste(body, collapse=" ")
  bodies <- append(bodies, one_body)
  
}

market7hrs$Title <- titles
market7hrs$Body <- bodies

names(market7hrs)
market7hrs$Title
market7hrs$Body[1]

#Cleans the data to ensure that only texts that we want are there
drops <- c("WebPg","DateTime","DiffHours")
final_text <-market7hrs[ , !(names(market7hrs) %in% drops)]

#custom stop words
cust_stop <- data.frame(word=c("published", "2020", "a.m" , "p.m","ET"),
                        lexicon=c("cust","cust","cust","cust","cust"))

#Creating a frequency of words
stocks_tokens <- final_text %>%
  unnest_tokens(word, Body) %>%
  anti_join(stop_words) %>%
  anti_join(cust_stop) 

stocks_frequencycount <- stocks_tokens %>%
  count(word, sort=TRUE)

stocks_proportions <- stocks_frequencycount %>% 
  mutate(proportion = n/sum(n))%>%
  select(-n)

############################################
## Sentiment analysis 
#############################################
tidy_stocks_affin <- stocks_tokens %>%
  inner_join(get_sentiments("afinn"))%>%
  group_by(Title) %>% #if you remove the group_by it will calculate sentiment for all the data
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")%>%
  arrange(desc(sentiment))

tidy_stocks_bing <- stocks_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T)%>%
  #mutate(proportion = n/sum(n))%>%
  #select(-n)%>%
  arrange(desc(n)) #change to proportion if proportion wanted

tidy_stocks_nrc <- stocks_tokens %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=T)%>%
  arrange(desc(n))

stocks_bing <- tidy_stocks_bing %>%
  group_by(sentiment) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip() 

print(stocks_bing)

stocks_nrc <- stocks_tokens %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=50,
                   scale = c(0.5,0.5),
                   fixed.asp=TRUE,
                   title.size=1
  )

stocks_bing <- stocks_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("red", "green"),
                   max.words=50,
                   scale = c(0.7,0.7),
                   fixed.asp=TRUE,
                   title.size=1
  )

####### Bi Grams #######
stock_bigrams_count <-  final_text %>%
  unnest_tokens(bigram, Body, token = "ngrams", n=2)%>%
  separate(bigram, c("word1", "word2"), sep = " ")%>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% cust_stop$word)%>%
  filter(!word2 %in% cust_stop$word) %>%
  count(word1, word2, sort = TRUE)

stock_bigrams <-  final_text %>%
  unnest_tokens(bigram, Body, token = "ngrams", n=2)%>%
  separate(bigram, c("word1", "word2"), sep = " ")%>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% cust_stop$word)%>%
  filter(!word2 %in% cust_stop$word)%>%
  unite(bigram, word1, word2, sep=" ")

##############tf_idf##################
stock_tf_idf <- stock_bigrams %>%
  count(Title, bigram) %>%
  bind_tf_idf(bigram, Title, n) %>%
  arrange(desc(tf_idf))
stock_tf_idf%>%
  ggplot(aes(bigram, tf_idf, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip() 

###########Bar charts of bi grams###########

stock_tf_idf%>%
  arrange(desc(tf_idf))%>%
  mutate(word=factor(bigram, levels=rev(unique(bigram)))) %>%
  mutate(word=reorder(word, tf_idf)) %>%
  group_by(Title) %>%
  top_n(5) %>%
  ungroup %>%
  ggplot(aes(bigram, tf_idf, fill=Title))+
  ggtitle("Key Word Pairs in Each Article")+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~Title, ncol=2, scales="free")+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())+
  coord_flip()

#############Visualizing Bigrams############
bigram_graph <- stock_bigrams_count %>%
  filter(n>2) %>%
  graph_from_data_frame()
bigram_graph
library(ggraph)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)
